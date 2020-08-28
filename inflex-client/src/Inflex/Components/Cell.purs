-- | A declaration in a document.

module Inflex.Components.Cell
  ( component
  , Cell(..)
  , Output(..)
  ) where

import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell.Editor as Editor
import Inflex.Components.Cell.Name as Name
import Inflex.Schema as Shared
import Prelude (Unit, discard, identity, pure, unit, (<<<))

--------------------------------------------------------------------------------
-- Component types

type Input = Shared.OutputCell

data Output
  = CellUpdate { name :: String, code :: String}
  | RemoveCell

data State = State
  { cell :: Cell
  , display :: Display
  }

data Command
  = SetCell Cell
  | CodeUpdate Cell
  | DeleteCell

--------------------------------------------------------------------------------
-- Internal types

data Cell = Cell
  { name :: String
  , code :: String
  , result :: Either String Editor.Editor
  }

data Display
  = DisplayResult
  | DisplayEditor String
  | DisplayTable (Set String) (Array (Map String String))

--------------------------------------------------------------------------------
-- Component

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState:
        (\cell ->
           State
             { cell: outputCellToCell cell
             , display: DisplayResult
             })
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetCell <<< outputCellToCell
            }
    }

outputCellToCell :: Shared.OutputCell -> Cell
outputCellToCell (Shared.OutputCell {name, code, result}) =
  Cell
    { name
    , code
    , result:
        case result of
          Shared.ResultError e -> Left e
          Shared.ResultOk output -> Right (Editor.MiscE output)
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    CodeUpdate (Cell {name, code}) -> do
      H.liftEffect (log "Inflex.Cell:CodeUpdate, raising ...")
      H.raise (CellUpdate {name, code})
    SetCell cell -> H.put (State {cell, display: DisplayResult})
    DeleteCell -> H.raise RemoveCell

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit, declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State {cell: Cell {name, code, result}, display}) =
  HH.div
    [HP.class_ (HH.ClassName "cell")]
    [ HH.div
        [HP.class_ (HH.ClassName "cell-header")]
        [ HH.div
            [HP.class_ (HH.ClassName "cell-name")]
            [ HH.slot
                (SProxy :: SProxy "declname")
                unit
                Name.component
                name
                (\name' ->
                   pure
                     (CodeUpdate
                        (Cell {name: name', result, code})))
            ]
        , HH.button
            [ HP.class_ (HH.ClassName "delete-cell")
            , HE.onClick (\_ -> pure DeleteCell)
            ]
            [HH.text "X"]
        ]
    , HH.div
        [HP.class_ (HH.ClassName "cell-body")]
        [ HH.slot
            (SProxy :: SProxy "editor")
            unit
            Editor.component
            (Editor.EditorAndCode
               { editor: either Editor.MiscE identity result
               , code: code
               })
            (\code' ->
               pure
                 (CodeUpdate (Cell {name, result, code: code'})))
        ]
    ]
