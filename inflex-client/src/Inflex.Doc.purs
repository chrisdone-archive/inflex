module Inflex.Doc (component) where

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class
import Halogen as H
import Halogen.HTML as HH
import Inflex.Dec as Dec
import Prelude

component :: forall q i o m. MonadEffect m =>  H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }

render =
  \state ->
    HH.div
      []
      (map
         (\dec@(Dec.Dec {name}) ->
            HH.slot
              (SProxy :: SProxy "doc")
              name
              Dec.component
              dec
              (const Nothing))
         decs)

decs =
  [Dec.Dec {name: "rate", rhs: "55.5"}
  ,Dec.Dec {name: "hours", rhs: "160"}
  ,Dec.Dec {name: "worked", rhs: "150"}
  ,Dec.Dec {name: "total", rhs: "worked / hours * rate"}]
