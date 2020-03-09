-- | A declaration in a document.

module Inflex.Dec
  ( component
  , Dec(..)
  ) where

import Data.Either (Either, either)
import Data.Map (Map)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Inflex.Editor as Editor
import Prelude (Unit, identity, pure, unit, (<<<))

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
    { initialState: (\dec -> State {dec, display: DisplayResult})
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval, receive = pure <<< SetDec }
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    CodeUpdate dec -> H.raise dec
    SetDec dec -> H.put (State {dec, display: DisplayResult})

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit | keys) m Command)
                  Command
render (State {dec: Dec {name, rhs, result}, display}) =
  HH.div
    [HP.class_ (HH.ClassName "dec")]
    [ HH.span [HP.class_ (HH.ClassName "dec-name")] [HH.text name]
    , HH.span [HP.class_ (HH.ClassName "dec-eq")] [HH.text "="]
    , HH.span
        [HP.class_ (HH.ClassName "dec-rhs")]
        [ HH.slot (SProxy :: SProxy "editor")
                  unit
                  Editor.component
                  (Editor.EditorAndCode {editor: either Editor.MiscE identity result, code: rhs})
                  (\rhs' -> pure (CodeUpdate (Dec {name,result,rhs: rhs'})))
        ]
    ]
