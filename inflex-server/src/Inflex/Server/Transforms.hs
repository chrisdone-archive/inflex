{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Well-structured updates/transforms to the AST.

module Inflex.Server.Transforms where

import           Inflex.Display ()
import           Inflex.Parser
import qualified Inflex.Schema as Shared
import           Inflex.Types
import qualified Inflex.Types as FieldE (FieldE(..))
import           RIO

applyUpdateToDocument :: Shared.Update -> Shared.InputDocument1 -> Shared.InputDocument1
applyUpdateToDocument =
  \case
    Shared.CellUpdate Shared.UpdateCell { uuid
                                        , update = Shared.UpdatePath { path
                                                                     , update = Shared.NewFieldUpdate newField
                                                                     }
                                        } ->
      addNewFieldToDocument uuid path newField
    Shared.CellUpdate Shared.UpdateCell { uuid
                                        , update = Shared.UpdatePath { path
                                                                     , update = Shared.DeleteFieldUpdate deleteField
                                                                     }
                                        } ->
      deleteFieldToDocument uuid path deleteField
    Shared.CellUpdate Shared.UpdateCell { uuid
                                        , update = Shared.UpdatePath { path
                                                                     , update = Shared.RenameFieldUpdate renameField
                                                                     }
                                        } ->
      renameFieldToDocument uuid path renameField

addNewFieldToDocument ::
     Shared.UUID
  -> Shared.DataPath
  -> Shared.NewField
  -> Shared.InputDocument1
  -> Shared.InputDocument1
addNewFieldToDocument uuid0 path Shared.NewField {name = name0} Shared.InputDocument1 {cells} =
  Shared.InputDocument1 {cells = fmap apply cells}
  where
    apply same@Shared.InputCell1 {..} =
      if uuid == uuid0
        then Shared.InputCell1
               {code = addNewFieldInCode path (FieldName name0) code, ..}
        else same

deleteFieldToDocument ::
     Shared.UUID
  -> Shared.DataPath
  -> Shared.DeleteField
  -> Shared.InputDocument1
  -> Shared.InputDocument1
deleteFieldToDocument uuid0 path Shared.DeleteField {name = name0} Shared.InputDocument1 {cells} =
  Shared.InputDocument1 {cells = fmap apply cells}
  where
    apply same@Shared.InputCell1 {..} =
      if uuid == uuid0
        then Shared.InputCell1
               {code = deleteFieldInCode path (FieldName name0) code, ..}
        else same

renameFieldToDocument ::
     Shared.UUID
  -> Shared.DataPath
  -> Shared.RenameField
  -> Shared.InputDocument1
  -> Shared.InputDocument1
renameFieldToDocument uuid0 path Shared.RenameField {from,to=to0} Shared.InputDocument1 {cells} =
  Shared.InputDocument1 {cells = fmap apply cells}
  where
    apply same@Shared.InputCell1 {..} =
      if uuid == uuid0
        then Shared.InputCell1
               {code = renameFieldInCode path (FieldName from) (FieldName to0) code, ..}
        else same

-- TODO: Do something about errors occurring. And possibly type-check
-- the result of the document afterwards?

addNewFieldInCode :: Shared.DataPath -> FieldName -> Text -> Text
addNewFieldInCode path0 name code =
  case parseText "" code of
    Left {} -> code
    Right expr -> textDisplay (go path0 expr)
  where
    go :: Shared.DataPath -> Expression Parsed -> Expression Parsed
    go path =
      \case
        ArrayExpression array@Array {expressions}
          | Shared.DataElemOf _index path' <- path ->
            ArrayExpression array {expressions = fmap (go path') expressions}
        RecordExpression record@Record {fields}
          | Shared.DataFieldOf index path' <- path ->
            RecordExpression
              record
                { fields =
                    fmap
                      (\(i, fielde@FieldE {expression}) ->
                         fielde
                           { FieldE.expression =
                               if i == index
                                 then go path' expression
                                 else expression
                           })
                      (zip [0 ..] fields)
                }
        RecordExpression record@Record {fields, location}
          | Shared.DataHere <- path ->
            RecordExpression
              record
                { fields =
                    fields <>
                    [ FieldE
                        { name
                        , expression = HoleExpression Hole {location, typ = Nothing}
                        , location
                        }
                    ]
                }
        e -> e

renameFieldInCode :: Shared.DataPath -> FieldName -> FieldName -> Text -> Text
renameFieldInCode path0 from to' code =
  case parseText "" code of
    Left {} -> code
    Right expr -> textDisplay (go path0 expr)
  where
    go :: Shared.DataPath -> Expression Parsed -> Expression Parsed
    go path =
      \case
        ArrayExpression array@Array {expressions}
          | Shared.DataElemOf _index path' <- path ->
            ArrayExpression array {expressions = fmap (go path') expressions}
        RecordExpression record@Record {fields}
          | Shared.DataFieldOf index path' <- path ->
            RecordExpression
              record
                { fields =
                    fmap
                      (\(i, fielde@FieldE {expression}) ->
                         fielde
                           { FieldE.expression =
                               if i == index
                                 then go path' expression
                                 else expression
                           })
                      (zip [0 ..] fields)
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
    Right expr -> textDisplay (go path0 expr)
  where
    go :: Shared.DataPath -> Expression Parsed -> Expression Parsed
    go path =
      \case
        ArrayExpression array@Array {expressions}
          | Shared.DataElemOf _index path' <- path ->
            ArrayExpression array {expressions = fmap (go path') expressions}
        RecordExpression record@Record {fields}
          | Shared.DataFieldOf index path' <- path ->
            RecordExpression
              record
                { fields =
                    fmap
                      (\(i, fielde@FieldE {expression}) ->
                         fielde
                           { FieldE.expression =
                               if i == index
                                 then go path' expression
                                 else expression
                           })
                      (zip [0 ..] fields)
                }
        RecordExpression record@Record {fields}
          | Shared.DataHere <- path ->
            RecordExpression
              record
                { fields =
                    filter
                      (\FieldE {name} ->
                         name /= name0)
                      fields
                }
        e -> e
