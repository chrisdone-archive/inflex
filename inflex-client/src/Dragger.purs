-- | Dragging support.

module Dragger
  ( newDragger
  , Dragger
  , attach
  ) where

import Effect
import Prelude (Unit, class Show)
import Web.HTML.HTMLElement (HTMLElement)

foreign import data Dragger :: Type

instance showDragger :: Show Dragger where
  show _ = "Dragger"

foreign import newDragger :: HTMLElement -> Effect Dragger

foreign import attach
   :: HTMLElement -- Mouse actuation point
   -> HTMLElement -- Drag this
   -> Dragger     -- Using this dragger
   -> (Int -> Int -> Effect Unit)
   -- ^ Drag is complete, these are the new coordinates relative to the
   -- container element specified by newDragger.
   -> Effect Unit
