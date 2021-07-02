-- | Timed code. Prints to console the timing info.

module Timed where

import Data.Void

foreign import timed :: forall a. String -> (Void -> a) -> a
