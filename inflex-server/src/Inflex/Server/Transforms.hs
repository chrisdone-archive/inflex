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
    Shared.AddFieldUpdate newField -> addNewFieldToDocument newField
    _ -> error "TODO"

addNewFieldToDocument :: Shared.NewField -> Shared.InputDocument1 -> Shared.InputDocument1
addNewFieldToDocument Shared.NewField {path, name = name0, uuid = uuid0} Shared.InputDocument1 {cells} =
  Shared.InputDocument1 {cells = fmap apply cells}
  where
    apply same@Shared.InputCell1 {..} =
      if uuid == uuid0
        then Shared.InputCell1
               {code = addNewFieldInCode path (FieldName name0) code, ..}
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
