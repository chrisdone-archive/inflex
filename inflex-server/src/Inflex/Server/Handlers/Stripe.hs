{-# LANGUAGE ScopedTypeVariables #-}
-- | Handler for Stripe's webhooks.

module Inflex.Server.Handlers.Stripe where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Inflex.Server.App
import           Yesod

-- TODO: check sigs https://stripe.com/docs/webhooks/signatures

postStripeR :: Handler ()
postStripeR = do
  body :: Value <- requireCheckJsonBody
  liftIO (L8.putStrLn (encode body))
