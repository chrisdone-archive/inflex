module Inflex.App (component) where

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Inflex.Doc as Doc
import Prelude (const, unit)

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState: const unit
    , render:
        (\state ->
           HH.slot
             (SProxy :: SProxy "doc")
             unit
             Doc.component
             unit
             (const Nothing))
    , eval: H.mkEval H.defaultEval
    }
