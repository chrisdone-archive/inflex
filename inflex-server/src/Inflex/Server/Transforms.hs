{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Well-structured updates/transforms to the AST.

module Inflex.Server.Transforms
  ( applyRename
  , applyDelete
  , applyUpdateToDocument
  , applyReposition
  , TransformError(..)
  ) where

import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Vector as V
import           Inflex.Server.Compute
import           Inflex.Printer
import           Inflex.Parser
import qualified Inflex.Server.Compute as InputDocument (InputDocument(..))
import qualified Inflex.Server.Compute as InputCell (InputCell(..))
import qualified Inflex.Schema as Shared
import           Inflex.Types
import qualified Inflex.Types as Field (Field(..))
import qualified Inflex.Types as FieldE (FieldE(..))

data TransformError
  = TransformedParseError LexParseError
  | OriginalSourceNotParsing Shared.DataPath LexParseError Text
  deriving (Show)

--------------------------------------------------------------------------------
-- General dispatcher

applyRename :: Shared.RenameCell -> InputDocument -> InputDocument
applyRename (Shared.RenameCell {uuid = uuid0, newname}) inputDocument1 =
  inputDocument1
    { InputDocument.cells =
        fmap
          (\InputCell {..} ->
             InputCell
               { name =
                   if uuid == uuid0
                     then newname
                     else name
               , ..
               })
          (InputDocument.cells inputDocument1)
    }

applyReposition :: Shared.RepositionCell -> InputDocument -> InputDocument
applyReposition (Shared.RepositionCell {uuid = uuid0, x, y}) inputDocument1 =
  inputDocument1
    { InputDocument.cells =
        fmap
          (\InputCell {..} ->
             InputCell
               { position =
                   if uuid == uuid0
                     then pure (x,y)
                     else position
               , ..
               })
          (InputDocument.cells inputDocument1)
    }

applyDelete :: Shared.DeleteCell -> InputDocument -> InputDocument
applyDelete (Shared.DeleteCell {uuid = uuid0}) inputDocument1 =
  inputDocument1
    { InputDocument.cells =
        V.filter
          (\InputCell {..} -> uuid /= uuid0)
          (InputDocument.cells inputDocument1)
    }

applyUpdateToDocument ::
     Shared.UpdateCell
  -> InputDocument
  -> Either TransformError InputDocument
applyUpdateToDocument Shared.UpdateCell {uuid, update} =
  case cmd of
    Shared.NewFieldUpdate Shared.NewField {name = name0} ->
      mapUuid uuid (pure . addNewFieldInCode path (FieldName name0))
    Shared.DeleteFieldUpdate Shared.DeleteField {name = name0} ->
      mapUuid uuid (pure . deleteFieldInCode path (FieldName name0))
    Shared.RenameFieldUpdate Shared.RenameField {from, to = to0} ->
      mapUuid
        uuid
        (pure . renameFieldInCode path (FieldName from) (FieldName to0))
    Shared.CodeUpdate (Shared.Code code) ->
      case path of
        Shared.DataHere -> mapUuid uuid (const (pure code))
        _ -> mapUuidPath uuid path (MapExpression (setParsed code))
    Shared.AddToEndUpdate ->
      mapUuidPath uuid path (MapArray (pure . addArrayItem))
    Shared.RemoveUpdate (Shared.Removal {index}) ->
      mapUuidPath uuid path (MapArray (pure . removeArrayItem index))
  where
    Shared.UpdatePath {path, update = cmd} = update

--------------------------------------------------------------------------------
-- Code updaters

setParsed :: Text -> Expression Parsed -> Either TransformError (Expression Parsed)
setParsed new _orig = first TransformedParseError (parseText "" new)

addNewFieldInCode :: Shared.DataPath -> FieldName -> Text -> Text
addNewFieldInCode path0 name code =
  case parseText "" code of
    Left {} -> code
    Right expr -> printerText (go path0 expr)
  where
    go :: Shared.DataPath -> Expression Parsed -> Expression Parsed
    go path =
      \case
        VariantExpression variant@Variant { tag = TagName expected
                                          , argument = marg
                                          }
          | Shared.DataVariantOf actual path' <- path
          , actual == expected
          , Just expr <- marg -> do
            (VariantExpression variant {argument = Just (go path' expr)})
        ArrayExpression array@Array {expressions, location}
          | Shared.DataElemOf _index path' <- path ->
            ArrayExpression
              (withFields
                 (\fields ->
                    fields <> [Field {location, name, typ = FreshType location}])
                 (array {expressions = fmap (go path') expressions}))
        RecordExpression record@Record {fields}
          | Shared.DataFieldOf index path' <- path ->
            RecordExpression
              record
                { fields =
                    fmap
                      (\(fielde@FieldE {expression, name = FieldName name'}) ->
                         fielde
                           { FieldE.expression =
                               if name' == index
                                 then go path' expression
                                 else expression
                           })
                      fields
                }
        RecordExpression record@Record {fields, location}
          | Shared.DataHere <- path ->
            RecordExpression
              record
                { fields =
                    fields <>
                    [ FieldE
                        { name
                        , expression =
                            HoleExpression Hole {location, typ = Nothing}
                        , location
                        }
                    ]
                }
        e -> e

renameFieldInCode :: Shared.DataPath -> FieldName -> FieldName -> Text -> Text
renameFieldInCode path0 from to' code =
  case parseText "" code of
    Left {} -> code
    Right expr -> printerText (go path0 expr)
  where
    go :: Shared.DataPath -> Expression Parsed -> Expression Parsed
    go path =
      \case
        VariantExpression variant@Variant { tag = TagName expected
                                          , argument = marg
                                          }
          | Shared.DataVariantOf actual path' <- path
          , actual == expected
          , Just expr <- marg -> do
            (VariantExpression variant {argument = Just (go path' expr)})
        ArrayExpression array@Array {expressions}
          | Shared.DataElemOf _index path' <- path ->
            ArrayExpression
              (withFields
                 (\fields ->
                    map
                      (\f@Field {name} ->
                         if name == from
                           then f {Field.name = to'}
                           else f)
                      fields)
                 (array {expressions = fmap (go path') expressions}))
        RecordExpression record@Record {fields}
          | Shared.DataFieldOf index path' <- path ->
            RecordExpression
              record
                { fields =
                    fmap
                      (\(fielde@FieldE {expression, name = FieldName name}) ->
                         fielde
                           { FieldE.expression =
                               if name == index
                                 then go path' expression
                                 else expression
                           })
                      fields
                }
        RecordExpression record@Record {fields}
          | Shared.DataHere <- path ->
            RecordExpression
              record
                { fields =
                    fmap
                      (\field@FieldE {name} ->
                         if name == from
                           then field {FieldE.name = to'}
                           else field)
                      fields
                    -- ]
                }
        e -> e

deleteFieldInCode :: Shared.DataPath -> FieldName -> Text -> Text
deleteFieldInCode path0 name0 code =
  case parseText "" code of
    Left {} -> code
    Right expr -> printerText (go path0 expr)
  where
    go :: Shared.DataPath -> Expression Parsed -> Expression Parsed
    go path =
      \case
        VariantExpression variant@Variant { tag = TagName expected
                                          , argument = marg
                                          }
          | Shared.DataVariantOf actual path' <- path
          , actual == expected
          , Just expr <- marg -> do
            (VariantExpression variant {argument = Just (go path' expr)})
        ArrayExpression array@Array {expressions}
          | Shared.DataElemOf _index path' <- path ->
            ArrayExpression
              (withFields
                 (filter (not . (== name0) . Field.name))
                 (array {expressions = fmap (go path') expressions}))
        RecordExpression record@Record {fields}
          | Shared.DataFieldOf index path' <- path ->
            RecordExpression
              record
                { fields =
                    fmap
                      (\(fielde@FieldE {expression, name = FieldName name}) ->
                         fielde
                           { FieldE.expression =
                               if name == index
                                 then go path' expression
                                 else expression
                           })
                      fields
                }
        RecordExpression record@Record {fields}
          | Shared.DataHere <- path ->
            RecordExpression
              record {fields = filter (\FieldE {name} -> name /= name0) fields}
        e -> e

--------------------------------------------------------------------------------
-- Array

withFields :: ([Field Parsed] -> [Field Parsed]) -> Array Parsed -> Array Parsed
withFields f array@Array {typ}
  | Just (ArrayType (RecordType (RowType (row@TypeRow {fields})))) <- typ =
    array { typ = Just (ArrayType (RecordType (RowType row {fields = f fields}))) }
withFields _f array = array

addArrayItem :: Array Parsed -> Array Parsed
addArrayItem array@Array {location, expressions, typ}
  | Just (ArrayType (RecordType (RowType (TypeRow {fields})))) <- typ =
    array
      { expressions =
          expressions <>
          pure
            (RecordExpression
               Record
                 { fields =
                     map
                       (\Field {name} ->
                          FieldE
                            { name
                            , expression =
                                HoleExpression Hole {location, typ = Nothing}
                            , location
                            })
                       fields
                 , location
                 , typ = Nothing
                 })
      }
  | Just (RecordExpression Record {fields}) <- expressions V.!? 0 =
    array
      { expressions =
          expressions <>
          pure
            (RecordExpression
               Record
                 { fields =
                     map
                       (\FieldE {name} ->
                          FieldE
                            { name
                            , expression =
                                HoleExpression Hole {location, typ = Nothing}
                            , location
                            })
                       fields
                 , location
                 , typ = Nothing
                 })
      }
  | otherwise =
    array
      { expressions =
          expressions <> pure (HoleExpression Hole {location, typ = Nothing})
      }

removeArrayItem :: Int -> Array Parsed -> Array Parsed
removeArrayItem idx array@Array {expressions} =
  array
    { expressions =
        V.ifilter (\i _ -> i /= idx) expressions
    }

--------------------------------------------------------------------------------
-- Generic walkers

data Mapping
  = MapArray (Array Parsed -> Either TransformError (Array Parsed))
  | MapRecord (Record Parsed -> Either TransformError (Record Parsed))
  | MapExpression (Expression Parsed -> Either TransformError (Expression Parsed))

-- | Change something at a path in a uuid in the document.
mapUuidPath ::
     Shared.UUID
  -> Shared.DataPath
  -> Mapping
  -> InputDocument
  -> Either TransformError InputDocument
mapUuidPath uuid path mapping = mapUuid uuid (mapPath path mapping)

-- | Change something at a uuid in the document.
mapUuid ::
     Shared.UUID
  -> (Text -> Either TransformError Text)
  -> InputDocument
  -> Either TransformError InputDocument
mapUuid uuid0 f InputDocument {cells} = do
  cells' <- traverse apply cells
  pure (InputDocument {cells = cells'})
  where
    apply same@InputCell {..} =
      if uuid == uuid0
        then do
          code' <- f code
          pure
            InputCell
              { code = code'
              , sourceHash = HashNotKnownYet -- IMPORTANT: This ensures cache invalidation.
              , ..
              }
        else pure same

-- | Change something at a path in the source code.
mapPath :: Shared.DataPath -> Mapping -> Text -> Either TransformError Text
mapPath path0 mapping code =
  case parseText "" code of
    Left err -> Left (OriginalSourceNotParsing path0 err code)
    Right expr -> do
      expr' <- go path0 expr
      pure (printerText expr')
  where
    go ::
         Shared.DataPath
      -> Expression Parsed
      -> Either TransformError (Expression Parsed)
    go path =
      \case
        VariantExpression variant@Variant { tag = TagName expected
                                          , argument = marg
                                          }
          | Shared.DataVariantOf actual path' <- path
          , actual == expected
          , Just expr <- marg -> do
            expr' <- go path' expr
            pure (VariantExpression variant {argument = Just expr'})
        ArrayExpression array@Array {expressions}
          | Shared.DataElemOf index path' <- path -> do
            expressions' <-
              V.imapM
                (\i e ->
                   if i == index
                     then go path' e
                     else pure e)
                expressions
            pure (ArrayExpression (array {expressions = expressions'}))
        RecordExpression record@Record {fields}
          | Shared.DataFieldOf index path' <- path -> do
            fields' <-
              traverse
                (\(fielde@FieldE {expression, name = FieldName name}) -> do
                   expression' <-
                     if name == index
                       then go path' expression
                       else pure expression
                   pure fielde {FieldE.expression = expression'})
                fields
            pure (RecordExpression record {fields = fields'})
        e
          | Shared.DataHere <- path ->
            case e of
              ArrayExpression array
                | MapArray mapArray <- mapping ->
                  fmap ArrayExpression (mapArray array)
              RecordExpression record
                | MapRecord mapRecord <- mapping ->
                  fmap RecordExpression (mapRecord record)
              _
                | MapExpression mapExpression <- mapping -> mapExpression e
              _ -> pure e
          | otherwise -> pure e
