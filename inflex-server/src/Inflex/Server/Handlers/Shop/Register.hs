{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Inflex.Server.Handlers.Shop.Register
  ( handleShopRegisterR
  , getCheckoutCreateR
  , getCheckoutCancelR
  , getCheckoutSuccessR
  ) where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Text (Text)
import           Data.Validation
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Server.App
import           Inflex.Server.Forge
import           Inflex.Server.Forms
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.Shop
import           Lucid
import           Lucid.Julius
import           Optics
import           Stripe
import           Yesod hiding (Html, Field, toHtml)
import           Yesod.Forge
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- State to page mapping

registerRedirect :: RegistrationState -> Handler a
registerRedirect =
  \case
    CreateCheckout {} -> redirect CheckoutCreateR
    EnterDetails {} -> redirect ShopRegisterR
    WaitingForStripe {} -> redirect CheckoutSuccessR

withRegistrationState ::
     Prism' RegistrationState a
  -> (SessionId -> a -> Handler (Html ()))
  -> Handler (Html ())
withRegistrationState theCons cont = do
  session <- assumeSession (Unregistered EnterDetails)
  case session of
    (Entity sessionId Session {sessionState = state}) ->
      case state of
        Unregistered registerState ->
          case preview theCons registerState of
            Nothing -> registerRedirect registerState
            Just a -> cont sessionId a
        Registered{} ->
          htmlWithUrl "You are already registered!"

--------------------------------------------------------------------------------
-- Registration form

handleShopRegisterR :: Handler (Html ())
handleShopRegisterR = withRegistrationState _EnterDetails go
  where
    go sessionId () = do
      submission <- generateForm verifiedRegisterForm
      case submission of
        NotSubmitted v -> htmlWithUrl (registerView v)
        Submitted parse -> do
          let Forge.Generated {generatedView = v, generatedValue = result} =
                runIdentity parse
          case result of
            Failure _errors -> htmlWithUrl (registerView v)
            Success registrationDetails -> do
              runDB
                (updateSession
                   sessionId
                   (Unregistered (CreateCheckout registrationDetails)))
              redirect CheckoutCreateR

registerView :: Lucid App () -> Lucid App ()
registerView formView =
  shopTemplate
    (do url <- ask
        h1_ "Register"
        form_
          [action_ (url ShopRegisterR), method_ "POST", novalidate_ ""]
          (do formView
              p_ (button_ "Continue")))

verifiedRegisterForm :: VerifiedForm Error RegistrationDetails
verifiedRegisterForm = $$($$(Forge.verify [||registerForm||]))

--------------------------------------------------------------------------------
-- Checkout create page

getCheckoutCreateR :: Handler (Html ())
getCheckoutCreateR = withRegistrationState _CreateCheckout go
  where
    go sessionId registrationDetails = do
      render <- getUrlRender
      Config {stripeConfig} <- fmap appConfig getYesod
      -- TODO: Set sessionId from _sessionId
      -- TODO: https://stripe.com/docs/payments/checkout/subscriptions/starting#prefilling-customer-data
      -- TODO: Set Trial flag https://stripe.com/docs/payments/checkout/subscriptions/starting#handling-checkout-trials
      result <-
        createSession
          StripeSession
            { stripeConfig
            , successUrl = render CheckoutSuccessR
            , cancelUrl = render CheckoutCancelR
            }
      case result of
        Left err -> error (show err) -- TODO:
        Right CreateSessionResponse {checkoutSessionId} -> do
          runDB
            (updateSession
               sessionId
               (Unregistered (WaitingForStripe registrationDetails)))
          htmlWithUrl
            (html_
               (do head_
                     (script_ [src_ "https://js.stripe.com/v3/"] ("" :: Text))
                   body_
                     (do h1_ "One moment"
                         noscript_
                           "Please enable JavaScript so that we can securely send you to Stripe."
                         p_ "Redirecting you to Stripe ..."
                         julius_
                           (stripeJavaScript
                              (publishableApiKey stripeConfig)
                              checkoutSessionId))))

stripeJavaScript :: PublishableApiKey -> CheckoutSessionId -> JavascriptUrl (Route App)
stripeJavaScript stripePublishableKey checkoutSessionId = [julius|
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

--------------------------------------------------------------------------------
-- Checkout cancel page

getCheckoutCancelR :: Handler (Html ())
getCheckoutCancelR = withRegistrationState _WaitingForStripe go
  where
    go _sessionId _registrationDetails =
      htmlWithUrl
        (do h1_ "Checkout cancelled"
            p_ "Cancelled!")

--------------------------------------------------------------------------------
-- Checkout success page

getCheckoutSuccessR :: Handler (Html ())
getCheckoutSuccessR = withRegistrationState _WaitingForStripe go
  where
    go _sessionId _registrationDetails =
      htmlWithUrl
        (do h1_ "Checkout succeeded!"
            p_ "Great success!"
            p_ "Now we go to the dashboard...")
