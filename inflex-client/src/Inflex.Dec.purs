-- | A declaration in a document.

module Inflex.Dec
  ( component
  , Dec(..)
  ) where

import Data.Either (Either, either)
import Data.Map (Map)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Inflex.Dec.Name as Dec.Name
import Inflex.Editor as Editor
import Prelude
import Effect.Console (log)

--------------------------------------------------------------------------------
-- Component types

type Input = Dec

type Output = Dec

data State = State
  { dec :: Dec
  , display :: Display
  }

data Command
  = SetDec Dec
  | CodeUpdate Dec

--------------------------------------------------------------------------------
-- Internal types

data Dec = Dec
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
        H.mkEval H.defaultEval {handleAction = eval, receive = pure <<< SetDec}
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    CodeUpdate dec -> do
      H.liftEffect (log "Inflex.Dec:CodeUpdate, raising ...")
      H.raise dec
    SetDec dec -> H.put (State {dec, display: DisplayResult})

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit, declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State {dec: Dec {name, rhs, result}, display}) =
  HH.div
    [HP.class_ (HH.ClassName "card mt-3 declaration ml-3 mb-3")]
    [ HH.div
        [HP.class_ (HH.ClassName "card-header")]
        [ HH.slot
            (SProxy :: SProxy "declname")
            unit
            Dec.Name.component
            name
            (\name' ->
               pure (CodeUpdate (Dec {name: name', result, rhs, new: false})))
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
               pure (CodeUpdate (Dec {name, result, rhs: rhs', new: false})))
        ]
    ]
