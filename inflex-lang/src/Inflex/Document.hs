{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Loading a document of cells.

module Inflex.Document
  ( loadDocument
  , evalDocument
  , evalEnvironment
  , defaultDocument
  , loadDocument1
  , defaultDocument1
  , evalEnvironment1
  , evalDocument1
  , EvaledExpression(..)
  , Toposorted(..)
  , LoadError(..)
  , DocumentReader(..)
  ) where

import           Control.DeepSeq
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Foldable
import           Data.Graph
import           Data.List.Extra
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Defaulter
import           Inflex.Generaliser
import           Inflex.Generator
import           Inflex.Hash
import           Inflex.Renamer
import           Inflex.Resolver
import           Inflex.Solver
import           Inflex.Stepper
import           Inflex.Types
import qualified RIO
import           RIO (RIO)

--------------------------------------------------------------------------------
-- Types

data LoadError
  = CycleError [Text]
  | RenameLoadError ParseRenameError
  | DuplicateName
  | LoadGenerateError (RenameGenerateError LoadError)
  | LoadSolveError (GenerateSolveError LoadError)
  | LoadGeneraliseError (SolveGeneraliseError LoadError)
  | LoadResolveError (GeneraliseResolveError LoadError)
  | LoadDefaulterError DefaulterError
  | LoadStepError (DefaultStepError LoadError)
  deriving (Show, Eq)

newtype Toposorted a = Toposorted {unToposorted :: [a]}
  deriving (Functor, Traversable, Foldable, Show, Eq, Ord, NFData)

data Context = Context
  { hashedCells :: Map Hash (Either LoadError LoadedExpression)
  , nameHashes :: Map Text (Either LoadError Hash)
  }

data LoadedExpression = LoadedExpression
  { resolvedExpression :: IsResolved (Expression Resolved)
  , renamedExpression :: Expression Renamed
  }

data EvaledExpression = EvaledExpression
  { resultExpression :: Expression Resolved
  , cell :: Cell1
  }

data DocumentReader = DocumentReader

--------------------------------------------------------------------------------
-- Top-level entry points

-- TODO: Add cell limit

-- | Load a document up to resolution.
loadDocument ::
     [Named Text]
  -> RIO DocumentReader (Toposorted (Named (Either LoadError (IsResolved (Expression Resolved)))))
loadDocument =
  fmap
    (fmap
       (fmap
          (second (\LoadedExpression {resolvedExpression} -> resolvedExpression)))) .
  loadDocument1

-- | Load a document up to resolution.
loadDocument1 ::
     [Named Text]
  -> RIO DocumentReader (Toposorted (Named (Either LoadError LoadedExpression)))
loadDocument1 =
  dependentLoadDocument . topologicalSortDocument . independentLoadDocument

-- | Construct an evaluation environment.
evalEnvironment ::
     Toposorted (Named (Either LoadError (IsResolved (Expression Resolved))))
  -> Map Hash (Expression Resolved)
evalEnvironment =
  M.fromList .
  mapMaybe
    (\Named {thing = result} ->
       case result of
         Right IsResolved {thing} -> pure (hashExpression thing, thing)
         Left {} -> Nothing) .
  toList

evalEnvironment1 ::
     Toposorted (Named (Either LoadError LoadedExpression))
  -> Map Hash (Expression Resolved)
evalEnvironment1 =
  evalEnvironment .
  fmap (fmap (second (\LoadedExpression {resolvedExpression} -> resolvedExpression)))

-- | Default the expressions in a document to cells.
defaultDocument ::
     Toposorted (Named (Either LoadError (IsResolved (Expression Resolved))))
  -> RIO DefaulterReader (Toposorted (Named (Either LoadError Cell)))
defaultDocument =
  traverse
    (traverse
       (\result ->
          case result of
            Left a -> pure (Left a)
            Right expression ->
              fmap
                (first LoadDefaulterError)
                (defaultResolvedExpression expression)))

-- | Default the expressions in a document to cells.
defaultDocument1 ::
     Toposorted (Named (Either LoadError LoadedExpression))
  -> RIO DefaulterReader (Toposorted (Named (Either LoadError Cell1)))
defaultDocument1 =
  traverse
    (traverse
       (\result ->
          case result of
            Left a -> pure (Left a)
            Right LoadedExpression {..} -> do
              fmap
                (bimap
                   LoadDefaulterError
                   (\Cell {..} -> Cell1 {renamed = renamedExpression, ..}))
                (defaultResolvedExpression resolvedExpression)))

-- | Evaluate the cells in a document. The expression will be changed.
evalDocument ::
     Map Hash (Expression Resolved)
  -> Toposorted (Named (Either LoadError Cell))
  -> RIO StepReader (Toposorted (Named (Either LoadError (Expression Resolved))))
evalDocument env =
  traverse
    (traverse
       (\result ->
          case result of
            Left e -> pure (Left e)
            Right cell -> fmap (first LoadStepError) (stepDefaulted env cell)))

-- | Evaluate the cells in a document. The expression will be changed.
evalDocument1 ::
     Map Hash (Expression Resolved)
  -> Toposorted (Named (Either LoadError Cell1))
  -> RIO StepReader (Toposorted (Named (Either LoadError EvaledExpression)))
evalDocument1 env =
  traverse
    (traverse
       (\result -> do
          case result of
            Left e -> pure (Left e)
            Right cell@Cell1 {..} -> do
              resultExpression0 <-
                fmap (first LoadStepError) (stepDefaulted env Cell {..})
              case resultExpression0 of
                Left e -> pure (Left e)
                Right resultExpression -> pure (Right EvaledExpression {..})))

--------------------------------------------------------------------------------
-- Document loading

-- | Lex, parse, rename -- can all be done per cell in parallel.
independentLoadDocument ::
     [Named Text] -> [Named (Either LoadError (IsRenamed (Expression Renamed)))]
independentLoadDocument names =
  parMap
    rseq
    (\Named {..} ->
       Named
         { thing =
             if not (T.null (T.strip name)) &&
                any
                  (\Named {uuid = uuid', name = name'} ->
                     uuid' /= uuid && name == name')
                  names -- TODO: Fix this O(n^2) operation
               then Left DuplicateName
               else first RenameLoadError (renameText (T.unpack name) thing)
         , ..
         })
    names

-- | Fill, generate, solve, generalize, resolve, default, step.
--
-- Must be done in order.
dependentLoadDocument ::
     Toposorted (Named (Either LoadError (IsRenamed (Expression Renamed))))
  -> RIO DocumentReader (Toposorted (Named (Either LoadError LoadedExpression)))
dependentLoadDocument = fmap snd . mapAccumM loadCell (Context mempty mempty)
  where
    loadCell ::
         Context
      -> Named (Either LoadError (IsRenamed (Expression Renamed)))
      -> RIO DocumentReader (Context, Named (Either LoadError LoadedExpression))
    loadCell Context {hashedCells, nameHashes} result = do
      namedMaybeCell <-
        traverse
          (\result' ->
             case result' of
               Left e -> pure (Left e)
               Right c -> resolveRenamedCell hashedCells nameHashes c)
          result
      let nameHashes' = M.insert name (fmap hashLoaded thing) nameHashes
            where
              Named {name, thing} = namedMaybeCell
          hashedCells' =
            case namedMaybeCell of
              Named {thing = Left {}} -> hashedCells
              Named {thing = Right loaded} ->
                M.insert (hashLoaded loaded) (Right loaded) hashedCells
          hashLoaded LoadedExpression {resolvedExpression = cell} =
            hashResolved cell
      pure
        ( Context {hashedCells = hashedCells', nameHashes = nameHashes'}
        , namedMaybeCell)

-- | Sort the named cells in the document by reverse dependency order.
topologicalSortDocument ::
     [Named (Either LoadError (IsRenamed a))]
  -> Toposorted (Named (Either LoadError (IsRenamed a)))
topologicalSortDocument =
  Toposorted . concatMap cycleCheck . stronglyConnCompR . map toNode
  where
    toNode named@Named {name, thing = result} =
      case result of
        Right IsRenamed {unresolvedGlobals} ->
          (named, name, Set.toList unresolvedGlobals)
        Left {} -> (named, name, mempty)
    cycleCheck =
      \case
        AcyclicSCC (named, _, _) -> [named]
        CyclicSCC nameds ->
          fmap
            (\(named, _, _) ->
               fmap
                 (const (Left (CycleError (map (\(_, name, _) -> name) nameds))))
                 named)
            nameds

--------------------------------------------------------------------------------
-- Individual cell loading

-- | Load a renamed cell.
resolveRenamedCell ::
     Map Hash (Either LoadError LoadedExpression)
  -> Map Text (Either LoadError Hash)
  -> IsRenamed (Expression Renamed)
  -> RIO DocumentReader (Either LoadError LoadedExpression)
resolveRenamedCell globalTypes globalHashes isRenamed@IsRenamed {thing = renamedExpression} = do
  hasConstraints <-
    pure
      (first
         LoadGenerateError
         (generateRenamed
            (fmap
               (fmap
                  (\LoadedExpression {resolvedExpression = IsResolved {scheme}} ->
                     scheme))
               globalTypes)
            globalHashes
            isRenamed))
  case hasConstraints of
    Left e -> pure (Left e)
    Right hasConstraints' -> do
      isSolved <-
        fmap
          (first LoadSolveError)
          (RIO.runRIO
             SolveReader {glogfunc = mempty}
             (solveGenerated hasConstraints'))
      case isSolved of
        Left e -> pure (Left e)
        Right isSolved' -> do
          isGeneralised <-
            fmap
              (first LoadGeneraliseError)
              (RIO.runRIO GeneraliseReader (generaliseSolved isSolved'))
          case isGeneralised of
            Left e -> pure (Left e)
            Right isGeneralised' -> do
              isResolved <-
                RIO.runRIO
                  ResolveReader
                  (fmap
                     (first LoadResolveError)
                     (resolveGeneralised isGeneralised'))
              case isResolved of
                Left e -> pure (Left e)
                Right isResolved' ->
                  pure
                    (Right
                       (LoadedExpression
                          {resolvedExpression = isResolved', renamedExpression}))
