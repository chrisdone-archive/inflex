-- | Timed code. Prints to console the timing info.

module Timed where

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (Void, bind, pure, discard, (-), Unit)

-- | Pure timed.
foreign import timed :: forall a. String -> (Void -> a) -> a

foreign import logg :: String -> Number -> Effect Unit

foreign import perfNow :: Effect Number

-- | Monadic timed.
timedM ::
     forall m a. MonadEffect m
  => String
  -> m a
  -> m a
timedM label m = do
  start <- liftEffect perfNow
  v <- m
  end <- liftEffect perfNow
  liftEffect (logg label (end-start))
  pure v
