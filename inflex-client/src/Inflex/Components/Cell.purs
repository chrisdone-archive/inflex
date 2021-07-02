-- | A declaration in a document.

module Inflex.Components.Cell
  ( component
  , Input(..)
  , Query(..)
  , Cell(..)
  , Output(..)
  ) where

import Inflex.Frisson

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell.Editor as Editor
import Inflex.Components.Cell.TextInput as TextInput
import Inflex.FieldName (validFieldName)
import Inflex.Schema as Shared
import Prelude (class Ord, class Show, Unit, bind, discard, identity, mempty, pure, show, unit, (<<<), (<>))
import Timed (timed)
import Web.HTML.Event.DragEvent as DE

--------------------------------------------------------------------------------
-- Component types

data Input = Input {
  cell :: View Shared.OutputCell,
  cells :: Map UUID (View Shared.OutputCell)
  }

data Query a
  = NestedCellError (View Shared.NestedCellError)

data Output
  = RemoveCell
  | UpdatePath Shared.UpdatePath
  | RenameCell String

data State = State
  { cell :: Cell
  , cells:: Map UUID (View Shared.OutputCell)
  }

data Command
  = SetCellFromInput Input
  | DeleteCell
  | TriggerUpdatePath Shared.UpdatePath
  | TriggerRenameCell String

derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show x = genericShow x

derive instance genericInput :: Generic Input _
instance showInput :: Show Input where show x = genericShow x

--------------------------------------------------------------------------------
-- Internal types

data Cell = Cell
  { name :: String
  , code :: String
  , result :: Either (View Shared.CellError) (View Shared.Tree2)
  , hash :: String
  }

derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where show x = genericShow x

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\(Input {cell, cells}) ->
           State {cell: outputCellToCell cell, cells})
    , render: \state -> timed "Cell.render" (\_ -> render state)
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetCellFromInput
            , handleQuery = query
            }
    }

outputCellToCell :: View Shared.OutputCell -> Cell
outputCellToCell cell =
  Cell
    { name: outputCellName cell
    , code: outputCellCode cell
    , hash: unHash (outputCellHash cell)
    , result:
        caseResult
          { "ResultError": Left
          , "ResultOk": \resultTree -> Right (unResultTree resultTree)
          }
          (outputCellResult cell)
    }

--------------------------------------------------------------------------------
-- Query

query ::
     forall a action output m t0 t1 x. Ord t1 => (MonadAff m)
  => Query a
  -> H.HalogenM State action (editor :: H.Slot Editor.Query t0 t1 | x) output m (Maybe a)
query =
  case _ of
    NestedCellError cellError -> do
      log ("[Cell] Received error:" <> show cellError)
      _ <- H.queryAll (SProxy :: SProxy "editor")
                 (Editor.NestedCellError cellError)
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

foreign import clearDragImage :: DE.DragEvent -> Effect Unit
foreign import setEmptyData :: DE.DragEvent -> Effect Unit

-- eval' :: forall q i m. MonadAff m =>  Command -> H.HalogenM State q i Output m Unit
-- eval' cmd = do
--   log (show cmd)
--   eval cmd

eval :: forall q i m. MonadAff m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    TriggerUpdatePath update -> H.raise (UpdatePath update)
    TriggerRenameCell update -> H.raise (RenameCell update)
    SetCellFromInput (Input {cell: c, cells}) -> do
      let cell@(Cell {hash, name}) = outputCellToCell c
      H.modify_
        (\(State s) -> State (s {cell = cell, cells = cells}))
    DeleteCell -> H.raise RemoveCell

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadAff m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot Editor.Query Editor.Output Unit,
                                             declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State { cell: Cell {name, code, result, hash}
              , cells
              }) =
  HH.div
    [HP.class_ (HH.ClassName "cell-wrapper")]
    [ HH.div
        [HP.class_ (HH.ClassName "cell")]
        [ HH.div
            [HP.class_ (HH.ClassName "cell-header")]
            [ HH.slot
                (SProxy :: SProxy "declname")
                unit
                (TextInput.component
                   (TextInput.Config
                      { placeholder: "Type a name here"
                      , unfilled: "(unnamed)"
                      , title: "Click to edit cell's name"
                      , validator: validFieldName
                      }))
                (TextInput.Input
                   {text: name, notThese: mempty})
                (\name' -> pure (TriggerRenameCell name'))
            , HH.button
                [ HP.class_ (HH.ClassName "delete-cell")
                , HE.onClick (\_ -> pure DeleteCell)
                , HP.title "Delete this cell"
                ]
                [HH.text "×"]
            ]
        , HH.div
            [HP.class_ (HH.ClassName "cell-body")]
            [ HH.slot
                (SProxy :: SProxy "editor")
                unit
                Editor.component
                (Editor.EditorAndCode
                   { editor: result
                   , code: code
                   , cells
                   , path: identity
                   })
                (\output ->
                   case output of
                     Editor.UpdatePath update -> Just (TriggerUpdatePath update)
                     Editor.NewCode text ->
                       Just
                         (TriggerUpdatePath
                            (Shared.UpdatePath
                               { path: Shared.DataHere
                               , update:
                                   Shared.CodeUpdate (Shared.Code {text})
                               })))
            ]
        ]
    ]
