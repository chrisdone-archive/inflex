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
  ( handleEnterDetailsR
  , getCheckoutCreateR
  , getCheckoutCancelR
  , getCheckoutSuccessR
  , getCheckoutWaitingR
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
import           Inflex.Server.Lucid
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

registerRedirect :: RegistrationState -> Handler (Html ())
registerRedirect state =
  case state of
    CreateCheckout {} -> redirect' CheckoutCreateR
    EnterDetails {} -> redirect' EnterDetailsR
    WaitingForStripe {} -> redirect' CheckoutWaitingR
    CheckoutSucceeded {} -> redirect' CheckoutSuccessR
  where
    redirect' route =
      htmlWithUrl
        (do url <- ask
            p_ (do "Wrong state constructor: "
                   code_ (toHtml (show state)))
            p_
              (a_
                 [href_ (url route)]
                 (do "Redirect to "
                     toHtml (url route))))

withRegistrationState ::
     Prism' RegistrationState a
  -> (SessionId -> a -> Handler (Html ()))
  -> Handler (Html ())
withRegistrationState theCons cont = do
  session <- assumeSession (Unregistered (EnterDetails Nothing))
  case session of
    (Entity sessionId Session {sessionState = state}) ->
      case state of
        Unregistered registerState ->
          case preview theCons registerState of
            Nothing -> registerRedirect registerState
            Just a -> cont sessionId a
        Registered{} ->
          htmlWithUrl "You are already registered! Let's go to the dashboard."

--------------------------------------------------------------------------------
-- Registration form

handleEnterDetailsR :: Handler (Html ())
handleEnterDetailsR = withRegistrationState _EnterDetails go
  where
    go sessionId mRegistrationDetails = do
      submission <-
        generateForm
          (verifiedRegisterForm (Forge.maybeDefault mRegistrationDetails))
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
          [action_ (url EnterDetailsR), method_ "POST", novalidate_ ""] -- TODO: remove novalidate.
          (do formView
              p_ (button_ "Continue")))

verifiedRegisterForm :: Forge.Default RegistrationDetails -> VerifiedForm Error RegistrationDetails
verifiedRegisterForm = $$($$(Forge.verify1 [||registerForm||]))

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
            , successUrl = render CheckoutWaitingR
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
-- Checkout waiting page

getCheckoutWaitingR :: Handler (Html ())
getCheckoutWaitingR = withRegistrationState _WaitingForStripe go
  where
    go sessionId registrationDetails = do
      -- TODO: Actually check Stripe confirmation.
      runDB
        (updateSession
           sessionId
           (Unregistered (CheckoutSucceeded registrationDetails)))
      htmlWithUrl
        (do -- TODO: Only redirect on stripe confirmation.
            redirect_ 5 CheckoutSuccessR
            h1_ "Waiting for Stripe confirmation"
            p_ "Waiting!")

--------------------------------------------------------------------------------
-- Checkout cancel page

getCheckoutCancelR :: Handler (Html ())
getCheckoutCancelR = withRegistrationState _WaitingForStripe go
  where
    go sessionId registrationDetails = do
      runDB
        (updateSession
           sessionId
           (Unregistered (EnterDetails (pure registrationDetails))))
      htmlWithUrl
        (do h1_ "Checkout cancelled"
            p_ "Going back to registration..."
            redirect_ 3 EnterDetailsR)

--------------------------------------------------------------------------------
-- Checkout success page

getCheckoutSuccessR :: Handler (Html ())
getCheckoutSuccessR = withRegistrationState _CheckoutSucceeded go
  where
    go sessionId RegistrationDetails {..} = do
      _ <-
        runDB
          (do void
                (insertUniqueEntity
                   Account
                     { accountUsername = registerUsername
                     , accountPassword = registerPassword
                     , accountEmail = registerEmail
                     })
              updateSession sessionId Registered)
      htmlWithUrl
        (do h1_ "Checkout succeeded!"
            p_ "Great success!"
            p_ "Now we go to the dashboard...")
