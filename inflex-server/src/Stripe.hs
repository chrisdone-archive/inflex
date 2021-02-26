{-# LANGUAGE DeriveGeneric, QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Stripe API integration.

module Stripe where

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Lucid
import           Lucid.Julius
import           Network.HTTP.Simple
import           RIO
import           Text.Julius
import           Yesod.Lucid

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
  , customerEmail :: Text
  , clientReferenceId :: Text
  , mcustomerId :: Maybe Text
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

data CreateCustomerError =
  CreateCustomerError | CustomerBadJson JSONException
  deriving (Show)

data CreateCustomerResponse = CreateCustomerResponse
  { id :: Text
  } deriving (Show, Generic)
instance FromJSON CreateCustomerResponse

data CreatePortalError =
  CreatePortalError | PortalBadJson JSONException
  deriving (Show)

data CreatePortalResponse = CreatePortalResponse
  { url :: Text
  } deriving (Show, Generic)
instance FromJSON CreatePortalResponse

--------------------------------------------------------------------------------
-- Commands

createSession ::
     (MonadIO m, MonadThrow m)
  => StripeSession
  -> m (Either CreateSessionError CreateSessionResponse)
createSession StripeSession { stripeConfig = StripeConfig {secretApiKey, planId}
                            , successUrl
                            , cancelUrl
                            , customerEmail
                            , clientReferenceId
                            , mcustomerId
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
      -- TODO: Add https://stripe.com/docs/api/checkout/sessions/create#create_checkout_session-client_reference_id
      setRequestBodyURLEncoded
        ([ ("payment_method_types[]", "card")
        , ("subscription_data[items][][plan]", T.encodeUtf8 (unPlanId planId))
        , ("success_url", T.encodeUtf8 successUrl)
        , ("cancel_url", T.encodeUtf8 cancelUrl)
        , ("customer_email", T.encodeUtf8 customerEmail)
        , ("client_reference_id", T.encodeUtf8 clientReferenceId)
        ] <> [("customer", T.encodeUtf8 customerId) | Just customerId <- [mcustomerId]])

createCustomer ::
     (MonadIO m, MonadThrow m)
  => StripeConfig
  -> Text
  -> m (Either CreateCustomerError CreateCustomerResponse)
createCustomer StripeConfig {secretApiKey} email = do
  request <-
    fmap hydrate (parseRequest "https://api.stripe.com/v1/customers")
    -- TODO: Handle errors or set fields manually.
  fmap (Right . getResponseBody) (httpJSON request) -- TODO: Use robust HTTP with retries.
  where
    hydrate =
      setRequestMethod "POST" .
      setRequestSecure True .
      setRequestBasicAuth (T.encodeUtf8 (unSecretApiKey secretApiKey)) mempty .
      setRequestBodyURLEncoded
        [ ("email", T.encodeUtf8 email)
        ]

-- | https://stripe.com/docs/api/customer_portal/sessions/create
createPortal ::
     (MonadIO m, MonadThrow m)
  => StripeConfig
  -> Text
  -> Text
  -> m (Either CreatePortalError CreatePortalResponse)
createPortal StripeConfig {secretApiKey} customer returnUrl = do
  request <-
    fmap hydrate (parseRequest "https://api.stripe.com/v1/billing_portal/sessions")
    -- TODO: Handle errors or set fields manually.
  fmap (Right . getResponseBody) (httpJSON request) -- TODO: Use robust HTTP with retries.
  where
    hydrate =
      setRequestMethod "POST" .
      setRequestSecure True .
      setRequestBasicAuth (T.encodeUtf8 (unSecretApiKey secretApiKey)) mempty .
      setRequestBodyURLEncoded
        [ ("customer", T.encodeUtf8 customer)
        , ("return_url", T.encodeUtf8 returnUrl)
        ]

--------------------------------------------------------------------------------
-- JavaScript

-- TODO: display error messsage to the user (see JS comment below).
stripeCheckout_ :: PublishableApiKey -> CheckoutSessionId -> Lucid app ()
stripeCheckout_ stripePublishableKey checkoutSessionId = do
  script_ [src_ "https://js.stripe.com/v3/"] ("" :: Text)
  julius_ [julius|
    var stripe = Stripe(#{stripePublishableKey});
    setTimeout (function () {
    stripe.redirectToCheckout({
      sessionId: #{checkoutSessionId}
    }).then(function (result) {
      // If `redirectToCheckout` fails due to a browser or network
      // error, display the localized error message to your customer
      // using `result.error.message`.
      console.log(result);
    });
    }, 1000);
  |]
