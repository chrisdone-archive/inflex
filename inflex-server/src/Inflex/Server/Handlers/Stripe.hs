{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for Stripe's webhooks.

module Inflex.Server.Handlers.Stripe where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import           Data.Text (Text)
import           Data.UUID as UUID
import           Inflex.Server.App
import           Inflex.Server.Handlers.Register
import           Inflex.Server.Types
import           Yesod

data Event
  = CheckoutSessionCompleted NonceUUID CustomerId
  | UnknownEvent Text Object
   deriving (Show)

-- TODO: check sigs https://stripe.com/docs/webhooks/signatures
postStripeR :: Handler ()
postStripeR = do
  event :: Event <- requireCheckJsonBody
  liftIO (S8.putStrLn (S8.pack (show event)))
  case event of
    UnknownEvent {} -> pure ()
    CheckoutSessionCompleted nonce customerId ->
      getCheckoutSessionCompletedR nonce customerId

instance FromJSON Event where
  parseJSON =
    withObject
      "stripe event"
      (\o -> do
         typ <- o .: "type"
         case typ of
           "checkout.session.completed" -> do
             clientReferenceId <-
               do client_reference_id :: Text <-
                    ((.: "data") >=>
                     (.: "object") >=> (.: "client_reference_id"))
                      o
                  case UUID.fromText client_reference_id of
                    Nothing ->
                      fail
                        ("invalid session UUID in client_reference_id: " <>
                         show client_reference_id)
                    Just uuid -> pure (NonceUUID uuid)
             customerRef <-
               ((.: "data") >=> (.: "object") >=> (.: "customer")) o
             pure (CheckoutSessionCompleted clientReferenceId customerRef)
           _ -> pure (UnknownEvent typ o))
