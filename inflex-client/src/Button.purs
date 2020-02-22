module Button (component) where

import Halogen.HTML.Core
import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { enabled :: Boolean }

data Action = Toggle

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [] [
     HH.span [HP.class_ (ClassName "lhs")] [HH.input [HP.value "rate"]]
    ,HH.span [HP.class_ (ClassName "eq")] [HH.text "="]
    ,HH.span [HP.class_ (ClassName "rhs")] [HH.input [HP.value "55.5"]]
    ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st { enabled = not st.enabled }
