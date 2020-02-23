module Inflex.Dec
  ( component
  , Dec(..)
  ) where

import Data.Maybe
import Effect.Class
import Effect.Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (const, unit, pure)
import Web.UIEvent.KeyboardEvent as K

data Dec = Dec {
    name :: String
  , rhs :: String
  }

data State = State {
    name :: String
  , rhs :: String
  , result :: Maybe String
  , display :: Display
  }

data Display = DisplayResult | DisplayEditor

data Command = StartEditor | KeyCode String | FinishEditing

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Dec o m
component =
  H.mkComponent
    { initialState:
        (\(Dec {name, rhs}) -> (State {name, rhs, result: Nothing, display: DisplayResult}))
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval }
    }

render (State {name, rhs, result, display}) =
  HH.div
    [HP.class_ (HH.ClassName "dec")]
    [ HH.text name
    , HH.span [HP.class_ (HH.ClassName "eq")] [HH.text "="]
    , case display of
        DisplayEditor ->
          HH.input
            [ HP.value rhs
            , HP.class_ (HH.ClassName "editor")
            , HE.onKeyDown
                (\k ->
                   case K.code k of
                     "Enter" -> Just FinishEditing
                     code -> Just (KeyCode code))
            ]
        DisplayResult ->
          HH.span [HE.onDoubleClick (\_ -> pure StartEditor)] [HH.text rhs]
    ]

eval =
  case _ of
    StartEditor ->
      H.modify_ (\(State st) -> State (st {display = DisplayEditor}))
    FinishEditing ->
      H.modify_ (\(State st) -> State (st {display = DisplayResult}))
    KeyCode code -> H.liftEffect (log code)
