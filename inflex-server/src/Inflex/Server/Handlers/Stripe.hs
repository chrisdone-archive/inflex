{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for Stripe's webhooks.

module Inflex.Server.Handlers.Stripe where

import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import           Data.Text (Text)
import           Data.UUID as UUID
import           Inflex.Server.App
import           Inflex.Server.Handlers.Register
import           Inflex.Server.Types
import           Yesod

data Event
  = CheckoutSessionCompleted SessionUUID CustomerId
  | UnknownEvent Text Object
   deriving (Show)

-- TODO: check sigs https://stripe.com/docs/webhooks/signatures
postStripeR :: Handler ()
postStripeR = do
  event :: Event <- requireCheckJsonBody
  liftIO (S8.putStrLn (S8.pack (show event)))
  case event of
    UnknownEvent {} -> pure ()
    CheckoutSessionCompleted sessionUUID customerId ->
      getCheckoutSessionCompletedR sessionUUID customerId

instance FromJSON Event where
  parseJSON =
    withObject
      "stripe event"
      (\o -> do
         typ <- o .: "type"
         case typ of
           "checkout.session.completed" -> withObject "data" (withObject "object" (\o' -> do
               clientReferenceId <-
                 do client_reference_id :: Text <-
                      o' .: "client_reference_id"
                    case UUID.fromText client_reference_id of
                      Nothing ->
                        fail
                          ("invalid session UUID in client_reference_id: " <>
                           show client_reference_id)
                      Just uuid -> pure (SessionUUID uuid)
               customerRef <- o' .: "customer"
               pure (CheckoutSessionCompleted clientReferenceId customerRef)) . Object) (Object o)
           _ -> pure (UnknownEvent typ o))
