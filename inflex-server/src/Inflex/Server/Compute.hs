{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Server.Compute where

import           Data.Foldable
import           Data.List
import qualified Data.Vector as V
import           Inflex.Display ()
import           Inflex.Document
import           Inflex.Instances ()
import           Inflex.Renamer
import qualified Inflex.Schema as Shared
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import           RIO

loadInputDocument :: Shared.InputDocument1 -> Shared.OutputDocument
loadInputDocument (Shared.InputDocument1 {cells}) =
  Shared.OutputDocument
    (V.fromList
       (sortBy
          (comparing (\Shared.OutputCell {order} -> order))
          ((fmap
              (\Named {uuid = Uuid uuid, name, thing, order, code} ->
                 Shared.OutputCell
                   { uuid = Shared.UUID uuid
                   , result =
                       either
                         (Shared.ResultError . toCellError)
                         (Shared.ResultOk . Shared.ResultTree . toTree)
                         thing
                   , code
                   , name
                   , order
                   })
              (unToposorted
                 (evalDocument (evalEnvironment loaded) (defaultDocument loaded)))))))
  where
    loaded =
      loadDocument
        (map
           (\Shared.InputCell1 {uuid = Shared.UUID uuid, name, code, order} ->
              Named {uuid = Uuid uuid, name, thing = code, order, code})
           (toList cells))

-- TODO: include code of each node in output where possible:
--
-- the renameText function produces a Map Cursor Location, which
-- toTree can use to compare with the cursors in the Expression
-- Resolved to produce the original source string for a given AST
-- node. genius!
--
-- so above in the outputcell, call renameText, give that map to this
-- function. update the Tree1 type to Tree2, including src code for
-- each node.
toTree :: Expression Resolved -> Shared.Tree1
toTree =
  \case
    ArrayExpression Array {expressions} ->
      Shared.ArrayTree Shared.versionRefl (fmap toTree expressions)
    RecordExpression Record {fields} ->
      Shared.RecordTree
        Shared.versionRefl
        (fmap
           (\FieldE {name = FieldName key, expression} ->
              Shared.Field1
                {key, version = Shared.versionRefl, value = toTree expression})
           (V.fromList fields))
    expression -> Shared.MiscTree Shared.versionRefl (textDisplay expression)

toCellError :: LoadError -> Shared.CellError
toCellError =
  \case
    CycleError names -> Shared.CyclicCells (V.fromList names)
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
                     OtherCellError name _ -> Shared.OtherCellProblem name)
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
