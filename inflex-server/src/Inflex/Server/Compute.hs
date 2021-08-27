{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -F -pgmF=early #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Server.Compute where

import           Control.Early
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Function
import           Data.Functor.Contravariant
import qualified Data.Graph as Graph
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Inflex.Defaulter
import           Inflex.Display ()
import           Inflex.Document
import           Inflex.Instances ()
import           Inflex.Location
import           Inflex.Renamer
import qualified Inflex.Schema as Shared
import           Inflex.Server.App
import           Inflex.Stepper
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import           Inflex.Types.SHA512
import           Prelude hiding (putStrLn)
import qualified RIO
import           RIO (HasGLogFunc(..), RIO, glog)
import           Yesod (getYesod)

milliseconds :: Int
milliseconds = 20_000

data OutputCell = OutputCell
  { uuid :: Shared.UUID
  , name :: Text
  , code :: Text
  , result :: Shared.Result
  , order :: Int
  , msourceHash :: Maybe SHA512
  , dependencies :: Set Uuid
  }

data InputDocument = InputDocument
  { cells :: Vector InputCell
  }

data InputCell = InputCell
  { uuid :: Shared.UUID
  , name :: Text
  , code :: Text
  , order :: Int
  , sourceHash :: SourceHash
  , dependencies :: Set Uuid
  }

fromInputDocument1 :: Shared.InputDocument1 -> InputDocument
fromInputDocument1 Shared.InputDocument1 {..} =
  InputDocument {cells = fmap fromInputCell1 cells}

fromInputCell1 :: Shared.InputCell1 -> InputCell
fromInputCell1 =
  \Shared.InputCell1 {..} -> InputCell {sourceHash = HashNotKnownYet, dependencies = mempty, ..}

loadInputDocument ::
     InputDocument
  -> Handler (Maybe (Vector OutputCell))
loadInputDocument (InputDocument {cells}) = do
  logfunc <- RIO.view gLogFuncL
  loadedCacheRef <- fmap appLoadCache getYesod
  loadedCache <- liftIO (readIORef loadedCacheRef)
  evalCacheRef <- fmap appEvalCache getYesod
  evalCache <- liftIO (readIORef evalCacheRef)
  -- Cache invalidation:
  --
  -- Here we generate a topologically sorted list, then iteratively
  -- insert the uuids into a set. If any of the cell's dependencies
  -- aren't present in this accumulating set, then don't include it.
  let cachedUuids =
        foldl'
          (\acc (_, uuid, deps) ->
             if null deps || all (\dep -> Set.member dep acc) deps
               then Set.insert uuid acc
               else acc)
          mempty
          (Graph.flattenSCCs
             (Graph.stronglyConnCompR
                (mapMaybe
                   (\InputCell { uuid = Shared.UUID uuid
                               , dependencies
                               , sourceHash
                               } ->
                      case sourceHash of
                        HashNotKnownYet -> Nothing
                        HashKnown {} ->
                          Just ((), Uuid uuid, toList dependencies))
                   (toList cells))))
  let inputCells =
        map
          (\InputCell { uuid = Shared.UUID uuid
                      , name
                      , code
                      , order
                      , sourceHash
                      , dependencies
                      } ->
             Named
               { uuid = Uuid uuid
               , name
               , thing = code
               , order
               , code
               , sourceHash =
                   if Set.member (Uuid uuid) cachedUuids
                      -- Only use the hash if we haven't been invalidated by other cells.
                     then sourceHash
                     else HashNotKnownYet
               , dependencies
               })
          (toList cells)
  for_ inputCells (\Named {name, sourceHash} -> glog (CellHash name sourceHash))
  loaded <-
    timed
      TimedLoadDocument1
      (RIO.runRIO
         DocumentReader {glogfunc = contramap LoadDocumentMsg logfunc}
         (RIO.timeout
            (1000 * milliseconds)
            (loadDocument1 loadedCache inputCells)))?
  liftIO
    (writeIORef
       loadedCacheRef
       (foldl'
          (\cache Named {thing} ->
             case thing of
               Right loadedExpression@LoadedExpression {..} ->
                 HM.insert (digestToSha512 sourceHash) loadedExpression cache
               Left {} -> cache)
          loadedCache
          loaded))
  defaulted <-
    timed
      TimedDefaulter
      (RIO.runRIO
         DefaulterReader
         (RIO.timeout (1000 * milliseconds) (defaultDocument1' loaded)))?
  topo <-
    timed
      TimedStepper
      (RIO.runRIO
         StepReader
         (RIO.timeout
            (1000 * milliseconds)
            (evalDocument1' evalCache (evalEnvironment1 loaded) defaulted)))?
  liftIO
    (writeIORef
       evalCacheRef
       (foldl'
          (\cache Named {thing, sourceHash} ->
             case thing of
               Right evalExpression@EvaledExpression {..}
                 | HashKnown hash <- sourceHash ->
                   HM.insert hash evalExpression cache
               _ -> cache)
          evalCache
          topo))
  outputCells <-
    RIO.pooledMapConcurrently
      (\Named {uuid = Uuid uuid, name, thing, order, code, dependencies} -> do
         glog (CellResultOk name (either (const False) (const True) thing))
         case thing of
           Left loadError -> glog (CellError loadError)
           _ -> pure ()
         let result =
               either
                 (Shared.ResultError . toCellError)
                 (\EvaledExpression {cell = Cell1 {parsed, mappings}, ..} ->
                    Shared.ResultOk
                      (Shared.ResultTree
                         { tree =
                             case parsed
                                  -- A temporary
                                  -- specialization to
                                  -- display lambdas in a
                                  -- cell as the original
                                  -- code. But, later,
                                  -- toTree will render
                                  -- lambdas structurally.
                                  of
                               LambdaExpression {} ->
                                 Shared.MiscTree2
                                   Shared.versionRefl
                                   (Shared.OriginalSource code)
                                   code
                               _ ->
                                 toTree mappings (pure parsed) resultExpression
                         , typ = Shared.MiscType -- TODO:
                         }))
                 thing
         glog (CellSharedResult result)
         pure
           (OutputCell
              { dependencies
              , uuid = Shared.UUID uuid
              , result
              , code
              , name
              , order
              , msourceHash =
                  case thing of
                    Left {} -> Nothing
                    Right EvaledExpression {cell = Cell1 {sourceHash}} ->
                      Just sourceHash
              }))
      (unToposorted topo)
  pure
    (Just
       (V.fromList
          (sortBy (comparing (\OutputCell {order} -> order)) outputCells)))
  where
    defaultDocument1' top = do
      !v <- defaultDocument1 top
      pure v

-- | Evaluate and force the result.
evalDocument1' ::
     HashMap SHA512 EvaledExpression
  -> RIO.Map Hash (Expression Resolved)
  -> Toposorted (Named (Either LoadError Cell1))
  -> RIO.RIO StepReader (Toposorted (Named (Either LoadError EvaledExpression)))
evalDocument1' cache env cells = do
  !v <- evalDocument1 cache env cells
  pure v

toTree ::
     (Map Cursor SourceLocation)
  -> Maybe (Expression Parsed)
  -> Expression Resolved
  -> Shared.Tree2
toTree mappings original final =
  case final of
    ArrayExpression Array {typ, expressions} -- Recognize a table.
      | ArrayType (RecordType (RowType TypeRow {fields})) <- typ ->
        Shared.TableTreeMaybe2
          Shared.versionRefl
          (originalSource inArray False)
          (V.fromList (map (\Field {name = FieldName text} -> text) fields))
          (let originalArray = inArray original
            in V.imap
                 (\rowIndex ->
                    \case
                      RecordExpression Record {fields = fieldEs} ->
                        let arrayItem = atIndex rowIndex originalArray
                            originalRecord = inRecord arrayItem
                         in Shared.SomeRow $
                            Shared.Row
                              { source =
                                  originalSource'
                                    id
                                    False -- TODO: review.
                                    arrayItem
                              , fields =
                                  let hash =
                                        HM.fromList
                                          (map
                                             (\(idx, FieldE { name = FieldName key
                                                            , expression
                                                            }) ->
                                                (key, (idx, expression)))
                                             (zip [0 ..] fieldEs))
                                   in V.map
                                        (\Field {name = FieldName key} ->
                                           maybe
                                             (error
                                                "TODO: Serious bug if this occurs. FIXME.")
                                             (\(fieldIndex, expression) ->
                                                toTree
                                                  mappings
                                                  (fmap
                                                     (\FieldE {expression = e} ->
                                                        e)
                                                     (atNth
                                                        fieldIndex
                                                        originalRecord))
                                                  expression)
                                             (HM.lookup key hash))
                                        (V.fromList fields)
                              }
                      _ ->
                        Shared.HoleRow (Shared.HoleTree (originalSource id True)))
                 expressions)
    ArrayExpression Array {expressions} ->
      Shared.ArrayTree2
        Shared.versionRefl
        (originalSource inArray False)
        (let originalArray = inArray original
          in V.imap
               (\i expression ->
                  toTree mappings (atIndex i originalArray) expression)
               expressions)
    RecordExpression Record {fields} ->
      Shared.RecordTree2
        Shared.versionRefl
        (originalSource inRecord False)
        (let originalRecord = inRecord original
          in V.imap
               (\i FieldE {name = FieldName key, expression} ->
                  Shared.Field2
                    { key
                    , version = Shared.versionRefl
                    , value =
                        toTree
                          mappings
                          (fmap
                             (\FieldE {expression = e} -> e)
                             (atNth i originalRecord))
                          expression
                    })
               (V.fromList fields))
    LiteralExpression (TextLiteral (LiteralText {text})) ->
      Shared.TextTree2 Shared.versionRefl (originalSource id False) text
    ApplyExpression Apply { typ = ConstantType TypeConstant {name = VegaTypeName}
                          , argument
                          } ->
      Shared.VegaTree2
        Shared.versionRefl
        Shared.NoOriginalSource
        (RIO.textDisplay argument)
    VariantExpression Variant {tag = TagName tag, argument} ->
      Shared.VariantTree2
        Shared.versionRefl
        Shared.NoOriginalSource
        tag
        (case argument of
           Just arg -> Shared.VariantArgument (toTree mappings Nothing arg)
           Nothing -> Shared.NoVariantArgument)
    HoleExpression {} -> Shared.HoleTree (originalSource id True)
    expression ->
      Shared.MiscTree2
        Shared.versionRefl
        (originalSource id True)
        (RIO.textDisplay expression)
  where
    inRecord :: Maybe (Expression Parsed) -> Maybe [FieldE Parsed]
    inRecord =
      \case
        Just (RecordExpression Record {fields}) -> pure fields
        _ -> Nothing
    inArray :: Maybe (Expression Parsed) -> Maybe (Vector (Expression Parsed))
    inArray =
      \case
        Just (ArrayExpression Array {expressions}) -> pure expressions
        _ -> Nothing
    atIndex ::
         Int -> Maybe (Vector (Expression Parsed)) -> Maybe (Expression Parsed)
    atIndex idx =
      \case
        Just vector
          | Just e <- vector V.!? idx -> pure e
        _ -> Nothing
    atNth :: Int -> Maybe [FieldE Parsed] -> Maybe (FieldE Parsed)
    atNth idx =
      \case
        Just vector
          | Just e <- lookup idx (zip [0 ..] vector) -> pure e
        _ -> Nothing
    originalSource ::
         (Maybe (Expression Parsed) -> Maybe a) -> Bool -> Shared.OriginalSource
    originalSource check atomic = originalSource' check atomic original
    originalSource' ::
         (Maybe (Expression Parsed) -> Maybe a)
      -> Bool
      -> Maybe (Expression Parsed)
      -> Shared.OriginalSource
    originalSource' check atomic e
      | Just {} <- check e =
        case e of
          Just expression
            | atomic -> Shared.OriginalSource (RIO.textDisplay expression)
            | Just sourceLocation <-
               M.lookup (expressionLocation final) mappings
            , sourceLocation == expressionLocation expression ->
              Shared.OriginalSource (RIO.textDisplay expression)
          _ -> Shared.NoOriginalSource
      | otherwise = Shared.NoOriginalSource

toCellError :: LoadError -> Shared.CellError
toCellError =
  \case
    CycleError _names -> Shared.CyclicCells mempty -- TODO: (V.fromList names)
    RenameLoadError parseRenameError -> parseRename parseRenameError
    DuplicateName -> Shared.DuplicateCellName
    LoadBadLex -> Shared.SyntaxError
    LoadGenerateError e ->
      case e of
        RenameGenerateError parseRenameError -> parseRename parseRenameError
        FillErrors errors ->
          Shared.FillErrors
            (V.fromList
               (map
                  (\case
                     MissingGlobal _ name -> Shared.NoSuchGlobal name
                     OtherCellError name _ -> Shared.OtherCellProblem name
                     MissingGlobalUuid _ (Uuid uuid) -> Shared.NoSuchGlobal uuid -- TODO: add constructor to shared
                     OtherCellUuidError (Uuid uuid) _ -> Shared.OtherCellProblem uuid) -- TODO:
                  (toList errors)))
        GeneratorErrors {} -> Shared.CellTypeError
    LoadSolveError {} -> Shared.CellTypeError
    LoadGeneraliseError {} -> Shared.CellTypeError
    LoadResolveError {} -> Shared.CellTypeError
    LoadDefaulterError {} -> Shared.CellTypeError
    LoadStepError {} -> Shared.CellStepEror
  where
    parseRename =
      \case
        RenamerErrors {} -> Shared.CellRenameErrors
        ParserErrored {} -> Shared.SyntaxError
