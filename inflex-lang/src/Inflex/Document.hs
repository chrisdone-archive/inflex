{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -F -pgmF=early #-}
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
  , DocumentMsg(..)
  ) where

import           Control.DeepSeq
import           Control.Early
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
import           Inflex.NormalFormCheck
import           Inflex.Optics
import qualified Inflex.Parser as Parser1
import qualified Inflex.Parser2 as Parser2
import           Inflex.Renamer
import           Inflex.Resolver
import           Inflex.Solver
import           Inflex.Stepper
import           Inflex.Types
import           Inflex.Types.Filler
import           Optics.Lens
import           Optics.TH
import qualified RIO
import           RIO (HasGLogFunc, GLogFunc, RIO)
import           RIO (glog, HasGLogFunc(..))

--------------------------------------------------------------------------------
-- Types

data LoadError
  = CycleError [Uuid]
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
  , nameHashes :: FillerEnv LoadError
  }

data LoadedExpression = LoadedExpression
  { resolvedExpression :: IsResolved (Expression Resolved)
  , parsedExpression :: Expression Parsed
  }

data EvaledExpression = EvaledExpression
  { resultExpression :: Expression Resolved
  , cell :: Cell1
  }

data DocumentReader = DocumentReader
  { glogfunc :: GLogFunc DocumentMsg
  }

data DocumentMsg
  = UsedNormalFormCodePath Text
  | UsedFullLexParseRenameCodePath Text
  deriving (Show)

$(makeLensesWith (inflexRules ['Inflex.Document.glogfunc, 'counter]) ''DocumentReader)

instance HasGLogFunc DocumentReader where
  type GMsg DocumentReader = DocumentMsg
  gLogFuncL = toLensVL documentReaderGlogfuncL

-- | This type represents the "independent" (parallel) loading of
-- source and whether we were able to do a "fast" load for arrays,
-- records, or the regular slow load use for all code (lambdas, case,
-- etc.).
data Independent
  = FullyResolvedNormalForm (IsResolved (Expression Resolved))
                            (Expression Parsed)
    -- ^ It was parsed with the fast parser and normal-form-checked
    -- all the way to resolved.
  | Renamed (IsRenamed (Expression Renamed))
            (Expression Parsed)
   -- ^ It's not necessarily a normal form checkable expression, so we
   -- renamed it.

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
                   (\Cell {..} -> Cell1 {parsed = parsedExpression, ..}))
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
     [Named Text] -> [Named (Either LoadError Independent)]
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
               else independentLoad (T.unpack name) thing
         , ..
         })
    names

-- | Load a cell's source as far as possible which can be done
-- independently of other cells.
independentLoad :: String -> Text -> Either LoadError Independent
independentLoad name source =
  case Parser2.parseText name source of
    Left Parser2.Failed ->
      first
        RenameLoadError
        (do expression <- first ParserErrored (Parser1.parseText name source)
            isRenamed <- first RenamerErrors (renameParsed expression)
            pure (Renamed isRenamed expression))
    -- A fast parse was possible. Let's try a fast type check.
    Right expression ->
      case resolveParsedResolved expression of
        Left {} ->
          first
            RenameLoadError
            (do isRenamed <- first RenamerErrors (renameParsed expression)
                pure (Renamed isRenamed expression))
        -- We got a successful type check and resolve. Use that.
        Right isResolved -> pure (FullyResolvedNormalForm isResolved expression)

-- | Fill, generate, solve, generalize, resolve, default, step.
--
-- Must be done in order.
dependentLoadDocument ::
     Toposorted (Named (Either LoadError Independent))
  -> RIO DocumentReader (Toposorted (Named (Either LoadError LoadedExpression)))
dependentLoadDocument =
  fmap snd . mapAccumM loadCell (Context mempty emptyFillerEnv)
  where
    loadCell ::
         Context
      -> Named (Either LoadError Independent)
      -> RIO DocumentReader (Context, Named (Either LoadError LoadedExpression))
    loadCell Context {hashedCells, nameHashes} result@Named{name} = do
      namedMaybeCell <-
        traverse
          (\result' ->
             case result' of
               Left e -> pure (Left e)
               Right (FullyResolvedNormalForm resolvedExpression parsedExpression) -> do
                 glog (UsedNormalFormCodePath name)
                 pure
                   (Right
                      LoadedExpression {resolvedExpression, parsedExpression})
               Right (Renamed renamed parsedExpression) -> do
                 glog (UsedFullLexParseRenameCodePath name)
                 isResolved <- resolveRenamedCell hashedCells nameHashes renamed
                 pure
                   (do resolvedExpression <- isResolved
                       pure
                         LoadedExpression {resolvedExpression, parsedExpression}))
          result
      let nameHashes' =
            insertNameAndUuid name uuid (fmap hashLoaded thing) nameHashes
            where
              Named {uuid, thing} = namedMaybeCell
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
     [Named (Either LoadError Independent)]
  -> Toposorted (Named (Either LoadError Independent))
topologicalSortDocument nameds =
  Toposorted . concatMap cycleCheck . stronglyConnCompR . map toNode $ nameds
  where
    toNode named@Named {uuid, thing = result} =
      case result of
        Right (Renamed IsRenamed {unresolvedGlobals, unresolvedUuids} _) ->
          ( named
          , uuid
          , Set.toList unresolvedUuids <>
            mapMaybe
              (\nameToLookup -> do
                 Named {uuid = foundUuid} <-
                   find (\Named {name = name'} -> nameToLookup == name') nameds
                 pure foundUuid)
              (Set.toList unresolvedGlobals))
        Right FullyResolvedNormalForm{} -> (named, uuid, mempty)
        Left {} -> (named, uuid, mempty)
    cycleCheck =
      \case
        AcyclicSCC (named, _, _) -> [named]
        CyclicSCC cyclicNameds ->
          fmap
            (\(named, _, _) ->
               fmap
                 (const (Left (CycleError (map (\(_, uuid, _) -> uuid) cyclicNameds))))
                 named)
            cyclicNameds

--------------------------------------------------------------------------------
-- Individual cell loading

-- | Load a renamed cell.
resolveRenamedCell ::
     Map Hash (Either LoadError LoadedExpression)
  -> FillerEnv LoadError
  -> IsRenamed (Expression Renamed)
  -> RIO DocumentReader (Either LoadError (IsResolved (Expression Resolved)))
resolveRenamedCell globalTypes globalHashes isRenamed = do
  hasConstraints <-
    pure $
    first LoadGenerateError $
    generateRenamed
      (fmap
         (fmap
            (\LoadedExpression {resolvedExpression = IsResolved {scheme}} ->
               scheme))
         globalTypes)
      globalHashes
      isRenamed?
  ref <- RIO.newSomeRef 0
  binds <- RIO.newSomeRef mempty
  isSolved <-
    fmap (first LoadSolveError) $
    RIO.runRIO
      SolveReader {glogfunc = mempty, counter = ref, binds}
      (solveGenerated hasConstraints)?
  isGeneralised <-
    fmap (first LoadGeneraliseError) $
    RIO.runRIO GeneraliseReader (generaliseSolved isSolved)?
  isResolved <-
    fmap (first LoadResolveError) $
    RIO.runRIO ResolveReader (resolveGeneralised isGeneralised)?
  pure (Right isResolved)
