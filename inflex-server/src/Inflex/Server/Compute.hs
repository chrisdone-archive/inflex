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
import qualified Data.Aeson as Aeson
import           Data.Foldable
import           Data.Functor.Contravariant
import           Data.List
import           Data.Ord
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Inflex.Defaulter
import           Inflex.Display ()
import           Inflex.Document
import           Inflex.Instances ()
import           Inflex.Renamer
import qualified Inflex.Schema as OutputCell (OutputCell(..))
import qualified Inflex.Schema as Shared
import           Inflex.Server.App
import           Inflex.Server.Types.Sha256
import           Inflex.Stepper
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import           Prelude hiding (putStrLn)
import qualified RIO
import           RIO (HasGLogFunc(..))
import           RIO (MonadIO, RIO)
import           RIO (glog)

milliseconds :: Int
milliseconds = 20_000

loadInputDocument ::
     (MonadIO m, HasGLogFunc env, RIO.MonadReader env m, GMsg env ~ ServerMsg)
  => Shared.InputDocument1
  -> m (Maybe Shared.OutputDocument)
loadInputDocument (Shared.InputDocument1 {cells}) = do
  logfunc <- RIO.view gLogFuncL
  loaded <-
    timed
      TimedLoadDocument1
      (RIO.runRIO
         DocumentReader {glogfunc = contramap LoadDocumentMsg logfunc}
         (RIO.timeout
            (1000 * milliseconds)
            (loadDocument1
               (map
                  (\Shared.InputCell1 { uuid = Shared.UUID uuid
                                      , name
                                      , code
                                      , order
                                      } ->
                     Named {uuid = Uuid uuid, name, thing = code, order, code})
                  (toList cells)))))?
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
    traverse
      (\Named {uuid = Uuid uuid, name, thing, order, code} -> do
         glog (CellResultOk name (either (const False) (const True) thing))
         pure
           (hashOutputCell
              (Shared.OutputCell
                 { uuid = Shared.UUID uuid
                 , hash = Shared.Hash mempty
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
                 })))
      (unToposorted topo)
  pure
    (Just
       (Shared.OutputDocument
          (V.fromList
             (sortBy
                (comparing (\Shared.OutputCell {order} -> order))
                outputCells))))
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

hashOutputCell :: Shared.OutputCell -> Shared.OutputCell
hashOutputCell cell =
  cell
    { OutputCell.hash =
        Shared.Hash (sha256AsHexText (sha256LazyByteString (Aeson.encode cell)))
    }

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
                         in Shared.SomeRow $ Shared.Row
                              { source = originalSource' arrayItem
                              , fields =
                                  V.imap
                                    (\fieldIndex FieldE { name = FieldName key
                                                        , expression
                                                        } ->
                                       Shared.Field2
                                         { key
                                         , version = Shared.versionRefl
                                         , value =
                                             toTree
                                               (fmap
                                                  (\FieldE {expression = e} -> e)
                                                  (atNth
                                                     fieldIndex
                                                     originalRecord))
                                               expression
                                         })
                                    (V.fromList fieldEs)
                              }
                      _ -> Shared.HoleRow)
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
         Int
      -> Maybe (Vector (Expression Parsed))
      -> Maybe (Expression Parsed)
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
