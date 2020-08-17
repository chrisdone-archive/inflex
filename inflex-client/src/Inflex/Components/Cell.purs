-- | A declaration in a document.

module Inflex.Components.Cell
  ( component
  , Cell(..)
  , Output(..)
  ) where

import Data.Either (Either, either)
import Data.Map (Map)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell.Name as Name
import Inflex.Components.Cell.Editor as Editor
import Prelude

--------------------------------------------------------------------------------
-- Component types

type Input = Cell

data Output = CelllUpdate Cell | DeleteCelll

data State = State
  { dec :: Cell
  , display :: Display
  }

data Command
  = SetCell Cell
  | CodeUpdate Cell
  | DeleteCelllaration

--------------------------------------------------------------------------------
-- Internal types

data Cell = Cell
  { name :: String
  , rhs :: String
  , result :: Either String Editor.Editor
  , new :: Boolean
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
        (\dec -> State {dec, display: DisplayResult})
    , render
    , eval:
        H.mkEval H.defaultEval {handleAction = eval, receive = pure <<< SetCell}
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    CodeUpdate dec -> do
      H.liftEffect (log "Inflex.Cell:CodeUpdate, raising ...")
      H.raise (CelllUpdate dec)
    SetCell dec -> H.put (State {dec, display: DisplayResult})
    DeleteCelllaration -> H.raise DeleteCelll

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit, declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State {dec: Cell {name, rhs, result}, display}) =
  HH.div
    [HP.class_ (HH.ClassName "card mt-3 declaration ml-3 mb-3")]
    [ HH.div
        [HP.class_ (HH.ClassName "card-header")]
        [ HH.slot
            (SProxy :: SProxy "declname")
            unit
            Name.component
            name
            (\name' ->
               pure
                 (CodeUpdate
                    (Cell
                       { name: name'
                       , result
                       , rhs
                       , new: false
                       })))
        , HH.button
            [ HP.class_ (HH.ClassName "btn btn-danger")
            , HE.onClick (\_ -> pure DeleteCelllaration)
            ]
            [HH.text "X"]
        ]
    , HH.div
        [HP.class_ (HH.ClassName "card-body")]
        [ HH.slot
            (SProxy :: SProxy "editor")
            unit
            Editor.component
            (Editor.EditorAndCode
               { editor: either Editor.MiscE identity result
               , code: rhs
               })
            (\rhs' ->
               pure
                 (CodeUpdate
                    (Cell
                       { name
                       , result
                       , rhs: rhs'
                       , new: false
                       })))
        ]
    ]
