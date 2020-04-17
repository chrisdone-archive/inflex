{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Stripe API integration.

module Stripe where

import           Data.Aeson
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Network.HTTP.Simple
import           RIO
import           Text.Julius

--------------------------------------------------------------------------------
-- Types

data StripeConfig = StripeConfig
  { publishableApiKey :: PublishableApiKey
  , planId :: PlanId
  , secretApiKey :: SecretApiKey
  } deriving (Generic)
instance FromJSON StripeConfig

newtype SecretApiKey = SecretApiKey
  { unSecretApiKey :: Text
  } deriving (FromJSON)
instance Show SecretApiKey where show _ = "SecretApiKey _"

newtype PlanId = PlanId
  { unPlanId :: Text
  } deriving (Show, FromJSON)

newtype PublishableApiKey = PublishableApiKey
  { unPublishableApiKey :: Text
  } deriving (Show, ToJSON, FromJSON)
instance ToJavascript PublishableApiKey where toJavascript = Javascript . LT.fromLazyText . LT.decodeUtf8 . encode

data StripeSession = StripeSession
  { stripeConfig :: StripeConfig
  , successUrl :: Text
  , cancelUrl :: Text
  }

newtype CheckoutSessionId = CheckoutSessionId
  { unCheckoutSessionId :: Text
  } deriving (Show, ToJSON, FromJSON)
instance ToJavascript CheckoutSessionId where toJavascript = Javascript . LT.fromLazyText . LT.decodeUtf8 . encode

data CreateSessionError =
  CreateSessionError | SessionBadJson JSONException
  deriving (Show)

data CreateSessionResponse = CreateSessionResponse
  { checkoutSessionId :: CheckoutSessionId
  } deriving (Show)
instance FromJSON CreateSessionResponse where
  parseJSON =
    withObject
      "CreateSessionResponse"
      (\o -> do
         checkoutSessionId <- o .: "id"
         pure CreateSessionResponse {..})

--------------------------------------------------------------------------------
-- Commands

createSession ::
     (MonadIO m, MonadThrow m)
  => StripeSession
  -> m (Either CreateSessionError CreateSessionResponse)
createSession StripeSession { stripeConfig = StripeConfig {secretApiKey, planId}
                            , successUrl
                            , cancelUrl
                            } = do
  request <-
    fmap hydrate (parseRequest "https://api.stripe.com/v1/checkout/sessions")
    -- TODO: Handle errors or set fields manually.
  fmap (Right . getResponseBody) (httpJSON request) -- TODO: Use robust HTTP with retries.
  where
    hydrate =
      setRequestMethod "POST" .
      setRequestSecure True .
      setRequestBasicAuth (T.encodeUtf8 (unSecretApiKey secretApiKey)) mempty .
      setRequestBodyURLEncoded
        [ ("payment_method_types[]", "card")
        , ("subscription_data[items][][plan]", T.encodeUtf8 (unPlanId planId))
        , ("success_url", T.encodeUtf8 successUrl)
        , ("cancel_url", T.encodeUtf8 cancelUrl)
        ]
