-- | Stringly-typed halogen elements and attributes.

module Halogen.Svg where

import Data.Maybe (Maybe(..))
import Halogen.HTML.Core (HTML, Prop)
import Halogen.HTML.Core as HC
import Halogen.VDom.Types as VD

elem :: forall a b. String -> Array (HC.Prop a) -> Array (HTML b a) -> HTML b a
elem key = HC.element (Just (VD.Namespace "http://www.w3.org/2000/svg")) (VD.ElemName key)

attr :: forall a. String -> String -> Prop a
attr key = HC.attr Nothing (HC.AttrName key)
