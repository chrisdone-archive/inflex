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
import           Data.Functor.Contravariant
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.List
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
  loadedCacheRef <- fmap appCache getYesod
  loadedCache <- liftIO (readIORef loadedCacheRef)
  loaded <-
    timed
      TimedLoadDocument1
      (RIO.runRIO
         DocumentReader {glogfunc = contramap LoadDocumentMsg logfunc}
         (RIO.timeout
            (1000 * milliseconds)
            (loadDocument1
               loadedCache
               (map
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
                           if all -- TODO: Make this less ugly and more efficient.
                                (\uuid0 ->
                                   case V.find
                                          (\InputCell { uuid = Shared.UUID uuid'
                                                      , sourceHash = sourceHash'
                                                      } ->
                                             Uuid uuid' == uuid0 &&
                                             sourceHash' /= HashNotKnownYet)
                                          cells of
                                     Nothing -> False
                                     Just {} -> True)
                                (Set.toList dependencies)
                             then sourceHash
                             else HashNotKnownYet
                       , dependencies
                       })
                  (toList cells)))))?
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
            (evalDocument1' (evalEnvironment1 loaded) defaulted)))?
  outputCells <-
    RIO.pooledMapConcurrently
      (\Named {uuid = Uuid uuid, name, thing, order, code, dependencies} -> do
         glog (CellResultOk name (either (const False) (const True) thing))
         pure
           (OutputCell
              { dependencies
              , uuid = Shared.UUID uuid
              , result =
                  either
                    (Shared.ResultError . toCellError)
                    (\EvaledExpression {cell = Cell1 {parsed}, ..} ->
                       Shared.ResultOk
                         (Shared.ResultTree
                            (case parsed
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
                               _ -> toTree (pure parsed) resultExpression)))
                    thing
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
     RIO.Map Hash (Expression Resolved)
  -> Toposorted (Named (Either LoadError Cell1))
  -> RIO.RIO StepReader (Toposorted (Named (Either LoadError EvaledExpression)))
evalDocument1' env cells = do
  !v <- evalDocument1 env cells
  pure v

toTree :: Maybe (Expression Parsed) -> Expression Resolved -> Shared.Tree2
toTree original =
  \case
    ArrayExpression Array {typ, expressions} -- Recognize a table.
      | ArrayType (RecordType (RowType TypeRow {fields})) <- typ ->
        Shared.TableTreeMaybe2
          Shared.versionRefl
          originalSource
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
                              { source = originalSource' arrayItem
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
                      _ -> Shared.HoleRow Shared.HoleTree)
                 expressions)
    ArrayExpression Array {expressions} ->
      Shared.ArrayTree2
        Shared.versionRefl
        originalSource
        (let originalArray = inArray original
          in V.imap
               (\i expression -> toTree (atIndex i originalArray) expression)
               expressions)
    RecordExpression Record {fields} ->
      Shared.RecordTree2
        Shared.versionRefl
        originalSource
        (let originalRecord = inRecord original
          in V.imap
               (\i FieldE {name = FieldName key, expression} ->
                  Shared.Field2
                    { key
                    , version = Shared.versionRefl
                    , value =
                        toTree
                          (fmap
                             (\FieldE {expression = e} -> e)
                             (atNth i originalRecord))
                          expression
                    })
               (V.fromList fields))
    LiteralExpression (TextLiteral (LiteralText {text})) ->
      Shared.TextTree2 Shared.versionRefl originalSource text
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
           Just arg -> Shared.VariantArgument (toTree Nothing arg)
           Nothing -> Shared.NoVariantArgument)
    expression ->
      Shared.MiscTree2
        Shared.versionRefl
        originalSource
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
    originalSource = originalSource' original
    originalSource' =
      \case
        Nothing -> Shared.NoOriginalSource
        Just expression -> Shared.OriginalSource (RIO.textDisplay expression)

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
