module Inflex.Dec
  ( component
  , Dec(..)
  ) where

import Data.Either
import Data.Maybe
import Effect.Class
import Effect.Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Web.UIEvent.KeyboardEvent as K

data Dec = Dec {
    name :: String
  , rhs :: String
  , result :: Either String String
  }

data State = State {
    dec :: Dec
  , display :: Display
  }

data Display = DisplayResult | DisplayEditor String

data Command = StartEditor | SetInput String | FinishEditing String | SetDec Dec

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Dec Dec m
component =
  H.mkComponent
    { initialState: (\dec -> State {dec, display: DisplayResult})
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval, receive = pure <<< SetDec }
    }

render (State {dec: Dec{name, rhs, result}, display}) =
  HH.div
    [HP.class_ (HH.ClassName "dec")]
    [ HH.text name
    , HH.span [HP.class_ (HH.ClassName "eq")] [HH.text " = "]
    , case display of
        DisplayEditor string ->
          HH.input
            [ HP.value string
            , HP.class_ (HH.ClassName "editor")
            , HE.onKeyUp
                (\k ->
                   case K.code k of
                     "Enter" -> Just (FinishEditing string)
                     code -> Nothing)
            , HE.onValueChange (\i -> pure (SetInput i))
            ]
        DisplayResult ->
          HH.span [HE.onClick (\_ -> pure StartEditor)] [HH.text (either identity identity result)]
    ]

eval =
  case _ of
    StartEditor ->
      H.modify_
        (\(State st) ->
           State
             (st
                { display =
                    DisplayEditor
                      (let Dec {rhs} = st . dec
                        in rhs)
                }))
    FinishEditing rhs -> do
      H.liftEffect (log ("Finish editing with rhs=" <> rhs))
      State st <- H.get
      let newDec =
            let Dec dec = st . dec
             in Dec (dec {rhs = rhs, result = Left "Waiting..."})
      H.modify_
        (\(State st') -> State (st' {display = DisplayResult, dec = newDec}))
      _result <- H.raise newDec
      H.modify_ (\(State st') -> State (st' {display = DisplayResult}))
    SetInput i ->
      H.modify_ (\(State st) -> State (st {display = DisplayEditor i}))
    SetDec dec -> H.put (State {dec, display: DisplayResult})
