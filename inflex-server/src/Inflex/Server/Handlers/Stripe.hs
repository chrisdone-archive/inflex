{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for Stripe's webhooks.

module Inflex.Server.Handlers.Stripe where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import           Data.Coerce
import           Data.Text (Text)
import           Data.UUID as UUID
import           GHC.Generics
import           Inflex.Server.App
import           Inflex.Server.Handlers.Portal
import           Inflex.Server.Types
import           RIO (glog)
import           Yesod

data Event
  = CheckoutSessionCompleted NonceUUID CustomerId
  | SubscriptionChanged Subscription
   deriving (Show)

data Subscription = Subscription
  { status :: Text
  , customer :: Text
  } deriving (Show, Generic)
instance FromJSON Subscription

-- TODO: check sigs https://stripe.com/docs/webhooks/signatures
postStripeR :: Handler ()
postStripeR = do
  event :: Event <- requireCheckJsonBody
  liftIO (S8.putStrLn (S8.pack (show event)))
  case event of
    CheckoutSessionCompleted nonce customerId ->
      getCheckoutSessionCompletedR nonce customerId
    SubscriptionChanged Subscription {customer, status} -> do
      glog (SubscriptionUpdated customer status (isSubscribed status))
      case isSubscribed status of
        Nothing -> pure ()
        Just subbed ->
          runDB
            (updateWhere
               [AccountCustomerId ==. coerce customer]
               [AccountSubscribed =. subbed])

-- | We're not interested in fine granularity at the moment, so we
-- compress all these states into a boolean.
isSubscribed :: Text -> Maybe Bool
isSubscribed =
  \case
    "incomplete" -> Nothing
    "incomplete_expired" -> pure False
    "canceled" -> pure False
    "unpaid" -> pure False
    "trialing" -> pure True
    "past_due" -> pure True
    "active" -> pure True
    _ -> Nothing

instance FromJSON Event where
  parseJSON =
    withObject
      "stripe event"
      (\o -> do
         typ <- o .: "type"
         case typ :: Text of
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
           "customer.subscription.created" -> fmap SubscriptionChanged ((o .: "data") >>= (.: "object"))
           "customer.subscription.deleted" -> fmap SubscriptionChanged ((o .: "data") >>= (.: "object"))
           "customer.subscription.updated" -> fmap SubscriptionChanged ((o .: "data") >>= (.: "object"))
           _ -> fail "unexpected event! check your webhook config.")
