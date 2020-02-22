module Button (component) where

import Halogen.HTML.Core
import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { enabled :: Boolean, enabled2 :: Boolean }

data Action = Clear | Focus | Focus2

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false, enabled2: false }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    []
    [ HH.div
        [HP.class_ (ClassName "nav")]
        [HH.span [HP.class_ (ClassName "logo")] [HH.text "Inflex"]]
    , HH.div
        [HP.class_ (ClassName "document"), HE.onClick (\_ -> Just Clear)]
        (map (\x -> HH.div [] [ HH.span
                        [ HP.class_
                            (ClassName
                               ("lhs" <>
                                (if state . enabled
                                   then " focused"
                                   else "")))
                        ]
                        [HH.span [HP.class_ (ClassName"input")] [HH.text (x.lhs)]]
                    , HH.span [HP.class_ (ClassName "eq")] [HH.text "="]
                    , HH.span
                        [ HP.class_
                            (ClassName
                               ("rhs" <>
                                (if state . enabled2
                                   then " focused"
                                   else "")))
                        ]
                        [HH.span[HP.class_ (ClassName"input")] [HH.text (x.rhs)]]
                    ])
             things)
    ]

things = [
  {lhs:"rate", rhs:"55.5"},
  {lhs:"standard_hours", rhs:"160"},
  {lhs:"hours_worked", rhs:"150"},
  {lhs:"due", rhs:"rate * (hours_worked / standard_hours)"}
  ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Clear ->
    H.modify_ \st -> st { enabled = false, enabled2 = false }
  Focus ->
    H.modify_ \st -> st { enabled = true }
  Focus2 ->
    H.modify_ \st -> st { enabled2 = true }
