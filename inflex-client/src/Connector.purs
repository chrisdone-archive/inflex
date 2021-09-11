-- | Keep SVG arrows connected to HTML elements.

module Connector
  ( newConnector
  ) where

import Effect
import Prelude (Unit)
import Web.HTML.HTMLElement (HTMLElement)

foreign import newConnector :: HTMLElement -> Effect Unit
