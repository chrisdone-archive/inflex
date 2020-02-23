module Inflex.App (component) where

import Prelude
import Halogen as H
import Halogen.HTML as HH

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render: (\state -> HH.div [] [])
    , eval: H.mkEval H.defaultEval
    }
