{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Server.Compute where

import           Data.Foldable
import           Data.Maybe
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
       (fmap
          (\Named {uuid = Uuid uuid, name, thing, order, code} ->
             Shared.OutputCell
               { uuid = Shared.UUID uuid
               , result =
                   either
                     (Shared.ResultError . toCellError)
                     (Shared.ResultOk . textDisplay)
                     thing
               , code
               , name
               , order
               })
          (unToposorted
             (evalDocument (evalEnvironment loaded) (defaultDocument loaded)))))
  where
    loaded =
      loadDocument
        (map
           (\Shared.InputCell1 {uuid = Shared.UUID uuid, name, code, order} ->
              Named {uuid = Uuid uuid, name, thing = code, order, code})
           (toList cells))

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
