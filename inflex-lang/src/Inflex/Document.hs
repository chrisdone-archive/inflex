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
  , Toposorted(..)
  , LoadError(..)
  ) where

import           Control.DeepSeq
import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Foldable
import           Data.Graph
import           Data.List
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
  { hashedCells :: Map Hash (Either LoadError (IsResolved (Expression Resolved)))
  , nameHashes :: Map Text (Either LoadError Hash)
  }

--------------------------------------------------------------------------------
-- Top-level entry points

-- | Load a document up to resolution.
loadDocument ::
     [Named Text]
  -> Toposorted (Named (Either LoadError (IsResolved (Expression Resolved))))
loadDocument =
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

-- | Default the expressions in a document to cells.
defaultDocument ::
     Toposorted (Named (Either LoadError (IsResolved (Expression Resolved))))
  -> Toposorted (Named (Either LoadError Cell))
defaultDocument =
  fmap
    (fmap
       (\result -> do
          expression <- result
          first LoadDefaulterError (defaultResolvedExpression expression)))

-- | Evaluate the cells in a document. The expression will be changed.
evalDocument ::
     Map Hash (Expression Resolved)
  -> Toposorted (Named (Either LoadError Cell))
  -> Toposorted (Named (Either LoadError (Expression Resolved)))
evalDocument env =
  fmap
    (fmap
       (\result -> do
          cell <- result
          first LoadStepError (stepDefaulted env cell)))

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
  -> Toposorted (Named (Either LoadError (IsResolved (Expression Resolved))))
dependentLoadDocument = snd . mapAccumL loadCell (Context mempty mempty)
  where
    loadCell ::
         Context
      -> Named (Either LoadError (IsRenamed (Expression Renamed)))
      -> (Context, Named (Either LoadError (IsResolved (Expression Resolved))))
    loadCell Context {hashedCells, nameHashes} result =
      ( Context {hashedCells = hashedCells', nameHashes = nameHashes'}
      , namedMaybeCell)
      where
        namedMaybeCell =
          fmap (>>= resolveRenamedCell hashedCells nameHashes) result
        nameHashes' = M.insert name (fmap hashResolved thing) nameHashes
          where
            Named {name, thing} = namedMaybeCell
        hashedCells' =
          case namedMaybeCell of
            Named {thing = Left {}} -> hashedCells
            Named {thing = Right cell} ->
              M.insert (hashResolved cell) (Right cell) hashedCells

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
     Map Hash (Either LoadError (IsResolved (Expression Resolved)))
  -> Map Text (Either LoadError Hash)
  -> IsRenamed (Expression Renamed)
  -> Either LoadError (IsResolved (Expression Resolved))
resolveRenamedCell globalTypes globalHashes isRenamed = do
  hasConstraints <-
    first
      LoadGenerateError
      (generateRenamed
         (fmap (fmap (\IsResolved {scheme} -> scheme)) globalTypes)
         globalHashes
         isRenamed)
  isSolved <- first LoadSolveError (solveGenerated hasConstraints)
  isGeneralised <- first LoadGeneraliseError (generaliseSolved isSolved)
  isResolved <- first LoadResolveError (resolveGeneralised isGeneralised)
  pure isResolved
