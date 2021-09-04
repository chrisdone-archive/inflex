-- |

module Halogen.Manage
  ( manage
  ) where

import Data.Maybe (Maybe(..))
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties as HP
import Halogen.Query.Input as Input
import Halogen.VDom.DOM.Prop (ElemRef)
import Prelude ((<<<))
import Web.DOM.Element (Element)

manage :: forall r i. (ElemRef Element -> i) -> HP.IProp r i
manage act = HP.IProp (Core.Ref (Just <<< Input.Action <<< act))
