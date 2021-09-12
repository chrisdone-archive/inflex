-- | Keep SVG arrows connected to HTML elements.

module Connector
  ( newConnector
  , Connector
  ) where

import Effect
import Prelude (Unit)
import Web.HTML.HTMLElement (HTMLElement)

foreign import data Connector :: Type

foreign import newConnector :: HTMLElement -> Effect Connector
