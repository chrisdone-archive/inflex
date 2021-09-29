{-# LANGUAGE TupleSections #-}
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
  , LoadedExpression(..)
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
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List.Extra
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Defaulter
import qualified Inflex.Eval as Eval
import           Inflex.Generaliser
import           Inflex.Generator
import           Inflex.Lexer
import           Inflex.NormalFormCheck
import           Inflex.Optics
import           Inflex.Parser
import qualified Inflex.Parser as Parser1
import qualified Inflex.Parser2 as Parser2
import           Inflex.Renamer
import           Inflex.Resolver
import           Inflex.Solver
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.SHA512
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
  | LoadBadLex
  | LoadGenerateError (RenameGenerateError LoadError)
  | LoadSolveError (GenerateSolveError LoadError)
  | LoadGeneraliseError (SolveGeneraliseError LoadError)
  | LoadResolveError (GeneraliseResolveError LoadError)
  | LoadDefaulterError DefaulterError
  | LoadStepError (Eval.DefaultEvalError LoadError)
  deriving (Show, Eq)

newtype Toposorted a = Toposorted {unToposorted :: [a]}
  deriving (Functor, Traversable, Foldable, Show, Eq, Ord, NFData)

data Context = Context
  { hashedCells :: !(Map Hash (Either LoadError LoadedExpression))
  , nameHashes :: !(FillerEnv LoadError)
  , uuidDigests :: !(Map Uuid Sha512Digest)
  }

data LoadedExpression = LoadedExpression
  { resolvedExpression :: IsResolved (Expression Resolved)
  , parsedExpression :: Expression Parsed
  , sourceHash :: Sha512Digest
    -- ^ A hash of the source and also any UUIDs upon which we depend.
  , uuids :: Set Uuid
    -- ^ UUIDs that we depend on.
  , mappings :: !(Map Cursor SourceLocation)
  , nameMappings :: !(Map Cursor Text)
  }

data EvaledExpression = EvaledExpression
  { resultExpression :: Expression Resolved
  , cell :: Cell1
  }

data DocumentReader = DocumentReader
  { glogfunc :: GLogFunc DocumentMsg
  }

data DocumentMsg
  = UsedFullyResolvedCodePath Text
  | UsedFullLexParseRenameCodePath Text
  | UsedLexerWithUUIDs Text
  | UsedCachedLoadedExpression Text
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
  = FullyResolved (IsResolved (Expression Resolved))
                  (Expression Parsed)
                  (Set Uuid)
    -- ^ It was parsed with the fast parser and normal-form-checked
    -- all the way to resolved.
  | CachedLoadedExpression LoadedExpression
    -- ^ It was cached so we have the data immediately.
  | Renamed (IsRenamed (Expression Renamed))
            (Expression Parsed)
   -- ^ It's not necessarily a normal form checkable expression, so we
   -- renamed it.
  | LexedWithUuids (Seq (Located Token))
                   (Set Uuid)

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
  loadDocument1 mempty

-- | Load a document up to resolution.
loadDocument1 ::
     HashMap SHA512 LoadedExpression
  -> [Named Text]
  -> RIO DocumentReader (Toposorted (Named (Either LoadError LoadedExpression)))
loadDocument1 cache =
  dependentLoadDocument . topologicalSortDocument . independentLoadDocument cache

-- | Construct an evaluation environment.
evalEnvironment ::
     Toposorted (Named (Either LoadError (IsResolved (Expression Resolved))))
  -> Map Hash (Expression Resolved)
evalEnvironment =
  M.fromList .
  mapMaybe
    (\Named {thing = result, sourceHash} ->
       case result of
         Right IsResolved {thing} | HashKnown sha512' <- sourceHash -> pure (Hash sha512', thing)
         _ -> Nothing) .
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
       (\result -> do
          expression <- pure result?
          fmap (first LoadDefaulterError) (defaultResolvedExpression expression)))

-- | Default the expressions in a document to cells.
defaultDocument1 ::
     Toposorted (Named (Either LoadError LoadedExpression))
  -> RIO DefaulterReader (Toposorted (Named (Either LoadError Cell1)))
defaultDocument1 =
  traverse
    (traverse
       (\result -> do
          LoadedExpression {..} <- pure result?
          fmap
            (bimap
               LoadDefaulterError
               (\Cell {..} ->
                  Cell1
                    { parsed = parsedExpression
                    , sourceHash = digestToSha512 sourceHash
                    , ..
                    }))
            (defaultResolvedExpression resolvedExpression)))

-- | Evaluate the cells in a document. The expression will be changed.
evalDocument ::
     Map Hash (Expression Resolved)
  -> Toposorted (Named (Either LoadError Cell))
  -> RIO Eval.Eval (Toposorted (Named (Either LoadError (Expression Resolved))))
evalDocument env =
  RIO.pooledMapConcurrently
    (traverse
       (\result -> do
          cell <- pure result?
          fmap
            (first LoadStepError)
            (RIO.runRIO
               Eval.Eval {globals = env, glogfunc = mempty}
               (Eval.evalDefaulted cell))))

-- | Evaluate the cells in a document. The expression will be changed.
evalDocument1 ::
     HashMap SHA512 EvaledExpression
  -> Map Hash (Expression Resolved)
  -> Toposorted (Named (Either LoadError Cell1))
  -> RIO Eval.Eval (Toposorted (Named (Either LoadError EvaledExpression)))
evalDocument1 cache env =
  RIO.pooledMapConcurrently
    (traverse
       (\result -> do
          cell@Cell1 {..} <- pure result?
          case HM.lookup sourceHash cache of
            Nothing -> do
              resultExpression <-
                fmap
                  (first LoadStepError)
                  (RIO.runRIO
                     Eval.Eval {globals = env, glogfunc = mempty}
                     (Eval.evalDefaulted Cell {..}))?
              pure (Right EvaledExpression {..})
            Just evaled -> pure (Right evaled)))

--------------------------------------------------------------------------------
-- Document loading

-- | Lex, parse, rename -- can all be done per cell in parallel.
independentLoadDocument ::
     HashMap SHA512 LoadedExpression
  -> [Named Text]
  -> [Named (Either LoadError Independent)]
independentLoadDocument cache names =
  parMap
    rseq
    (\Named {..} ->
       let independent =
             case sourceHash of
               HashKnown sha512'
                 | Just loadedExpression <- HM.lookup sha512' cache ->
                   Right (CachedLoadedExpression loadedExpression)
               _ -> independentLoad (T.unpack name) thing
           dependencies' =
             case independent of
               Left {} -> mempty
               Right indep ->
                 case indep of
                   FullyResolved _ _ uuids -> uuids
                   CachedLoadedExpression LoadedExpression {uuids} -> uuids
                   Renamed IsRenamed {unresolvedUuids} _ -> unresolvedUuids
                   LexedWithUuids _ uuids -> uuids
        in Named
             { thing =
                 if not (T.null (T.strip name)) &&
                    any
                      (\Named {uuid = uuid', name = name'} ->
                         uuid' /= uuid && name == name')
                      names -- TODO: Fix this O(n^2) operation
                   then Left DuplicateName
                   else independent
             , dependencies = dependencies'
             , ..
             })
    names

-- | Load a cell's source as far as possible which can be done
-- independently of other cells.
independentLoad :: String -> Text -> Either LoadError Independent
independentLoad name source =
  case Parser2.parseText name source of
    Left Parser2.Failed ->
      case lexTextPlusUUIDs source of
        Left _e -> Left LoadBadLex
        -- Fas parse wasn't possible, but a lex with the standard
        -- lexer was fine. Proceed from here.
        Right (tokens, uuids) -> pure (LexedWithUuids tokens uuids)
    -- A fast parse was possible. Let's try a fast type check.
    Right expression ->
      case resolveParsedResolved expression of
        Left {} ->
          first
            RenameLoadError
            -- No problem, we'll rename the fast-parsed AST instead.
            (do isRenamed <- first RenamerErrors (renameParsed expression)
                pure (Renamed isRenamed expression))
        -- We got a successful type check and resolve. Use that.
        Right isResolved -> pure (FullyResolved isResolved expression mempty)

-- | Fill, generate, solve, generalize, resolve, default, step.
--
-- Must be done in order.
dependentLoadDocument ::
     Toposorted (Named (Either LoadError Independent))
  -> RIO DocumentReader (Toposorted (Named (Either LoadError LoadedExpression)))
dependentLoadDocument =
  fmap updateHashes .
  mapAccumM
    loadCell
    Context
      {uuidDigests = mempty, hashedCells = mempty, nameHashes = emptyFillerEnv}
  where
    updateHashes ::
         (Context, Toposorted (Named (Either LoadError LoadedExpression)))
      -> Toposorted (Named (Either LoadError LoadedExpression))
    updateHashes (Context {uuidDigests}, nameds) =
      fmap
        (\Named {uuid, ..} ->
           Named
             { sourceHash =
                 case M.lookup uuid uuidDigests of
                   Nothing -> HashNotKnownYet
                   Just sha512' -> HashKnown (digestToSha512 sha512')
             , ..
             })
        nameds
    loadCell ::
         Context
      -> Named (Either LoadError Independent)
      -> RIO DocumentReader (Context, Named (Either LoadError LoadedExpression))
    loadCell Context {hashedCells, nameHashes, uuidDigests} result@Named { name
                                                                         , uuid
                                                                         , code
                                                                         } = do
      let makeSourceHash uuids =
            concatDigests
              (sha512DigestText code :
               mapMaybe
                 (\uuid' -> M.lookup uuid' uuidDigests)
                 (Set.toList uuids))
      namedMaybeCell <-
        traverse
          (\case
             Left e -> pure (Left e)
             Right (CachedLoadedExpression loadedExpression) -> do
               glog (UsedCachedLoadedExpression name)
               pure (Right loadedExpression)
             Right (FullyResolved resolvedExpression@IsResolved {mappings} parsedExpression uuids) -> do
               glog (UsedFullyResolvedCodePath name)
               pure
                 (Right
                    LoadedExpression
                      { uuids
                      , resolvedExpression
                      , parsedExpression
                      , sourceHash = makeSourceHash uuids
                      , mappings
                      , nameMappings = mempty -- The fast parser doesn't parse names.
                      })
             Right (LexedWithUuids tokens uuids) -> do
               glog (UsedLexerWithUUIDs name)
               case do parsed <-
                         first
                           (ParserErrored . ParseError)
                           (Parser1.parseTokens tokens)
                       fmap
                         (parsed, )
                         (first RenamerErrors (renameParsed parsed)) of
                 Left e -> pure (Left (RenameLoadError e))
                 Right (parsedExpression, renamed@IsRenamed{nameMappings}) -> do
                   isResolved <-
                     resolveRenamedCell hashedCells nameHashes renamed
                   pure
                     (do resolvedExpression@IsResolved {mappings} <-
                           isResolved
                         pure
                           LoadedExpression
                             { uuids
                             , resolvedExpression
                             , parsedExpression
                             , sourceHash = makeSourceHash uuids
                             , mappings
                             , nameMappings
                             })
             Right (Renamed renamed@IsRenamed { unresolvedUuids = uuids
                                              , mappings
                                              , nameMappings
                                              } parsedExpression) -> do
               glog (UsedFullLexParseRenameCodePath name)
               isResolved <- resolveRenamedCell hashedCells nameHashes renamed
               pure
                 (do resolvedExpression <- isResolved
                     pure
                       LoadedExpression
                         { uuids
                         , resolvedExpression
                         , parsedExpression
                         , sourceHash = makeSourceHash uuids
                         , mappings
                         , nameMappings
                         }))
          result
      let nameHashes' =
            insertNameAndUuid name uuid' (fmap hashLoaded thing) nameHashes
            where
              Named {uuid = uuid', thing} = namedMaybeCell
          hashedCells' =
            case namedMaybeCell of
              Named {thing = Left {}} -> hashedCells
              Named {thing = Right loaded} ->
                M.insert (hashLoaded loaded) (Right loaded) hashedCells
          hashLoaded LoadedExpression {sourceHash} =
            Hash (digestToSha512 sourceHash)
          uuidDigests' =
            case namedMaybeCell of
              Named {thing = Right LoadedExpression {sourceHash}} ->
                M.insert uuid sourceHash uuidDigests
              _ -> uuidDigests
      pure
        ( Context
            { uuidDigests = uuidDigests'
            , hashedCells = hashedCells'
            , nameHashes = nameHashes'
            }
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
        Right (LexedWithUuids _tokens uuids) -> (named, uuid, toList uuids)
        Right (CachedLoadedExpression LoadedExpression {uuids}) ->
          (named, uuid, toList uuids)
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
        Right FullyResolved {} -> (named, uuid, mempty)
        Left {} -> (named, uuid, mempty)
    cycleCheck =
      \case
        AcyclicSCC (named, _, _) -> [named]
        CyclicSCC cyclicNameds ->
          fmap
            (\(named, _, _) ->
               fmap
                 (const
                    (Left
                       (CycleError (map (\(_, uuid, _) -> uuid) cyclicNameds))))
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
