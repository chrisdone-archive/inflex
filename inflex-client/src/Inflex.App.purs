module Inflex.App (component) where

import Data.Maybe
import Data.Symbol (SProxy(..))
import Effect.Class
import Effect.Aff.Class
import Halogen as H
import Halogen.HTML as HH
import Inflex.Doc as Doc
import Prelude (const, unit)

component :: forall q i o m. MonadEffect m => MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render: (\state -> HH.slot (SProxy :: SProxy "doc") unit Doc.component unit (const Nothing))
    , eval: H.mkEval H.defaultEval
    }
