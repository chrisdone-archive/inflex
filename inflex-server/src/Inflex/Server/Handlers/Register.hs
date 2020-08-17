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

module Inflex.Server.Handlers.Register
  ( handleEnterDetailsR
  , getCheckoutCreateR
  , getCheckoutCancelR
  , getCheckoutSuccessR
  , getCheckoutWaitingR
  ) where

import           Control.Monad.Reader
import           Data.Text (Text)
import           Data.Validation
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Server.Lucid
import           Inflex.Server.App
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

registerRedirect :: RegistrationState -> Handler (Html ())
registerRedirect state =
  case state of
    CreateCheckout {} -> redirect' CheckoutCreateR
    EnterDetails {} -> redirect' EnterDetailsR
    WaitingForStripe {} -> redirect' CheckoutWaitingR
    CheckoutSucceeded {} -> redirect' CheckoutSuccessR
  where
    redirect' route =
      htmlWithUrl -- TODO: Fix the below for real-world use.
        (shopTemplate
           (Unregistered state)
           (containedColumn_
              (do url <- ask
                  p_
                    (do "Wrong state constructor: "
                        code_ (toHtml (show state)))
                  p_
                    (a_
                       [href_ (url route)]
                       (do "Redirect to "
                           toHtml (url route))))))

withRegistrationState ::
     Prism' RegistrationState a
  -> (SessionState -> SessionId -> a -> Handler (Html ()))
  -> Handler (Html ())
withRegistrationState theCons cont = do
  session <- assumeSession registerStart
  case session of
    (Entity sessionId Session {sessionState = state}) ->
      case state of
        Unregistered registerState ->
          case preview theCons registerState of
            Nothing -> registerRedirect registerState
            Just a -> cont state sessionId a
        Registered {} ->
          htmlWithUrl
            (shopTemplate
               state
               (containedColumn_
                  (do h1_ "Already registered!"
                      p_
                        "You are already registered! Taking you to the dashboard..."
                      spinner_
                      redirect_ 2 AppDashboardR)))
        NoSessionState {} -> do
          runDB (updateSession sessionId registerStart)
          redirect EnterDetailsR
  where
    registerStart = Unregistered (EnterDetails Nothing)

--------------------------------------------------------------------------------
-- Registration form

handleEnterDetailsR :: Handler (Html ())
handleEnterDetailsR = withRegistrationState _EnterDetails go
  where
    go state sessionId mRegistrationDetails = do
      submission <-
        generateForm
          (verifiedRegisterForm (Forge.maybeDefault mRegistrationDetails))
      case submission of
        NotSubmitted v -> htmlWithUrl (registerView state v)
        Submitted parse -> do
          Forge.Generated {generatedView = v, generatedValue = result} <-
            runDB parse
          case result of
            Failure _errors -> htmlWithUrl (registerView state v)
            Success registrationDetails -> do
              runDB
                (updateSession
                   sessionId
                   (Unregistered
                      ((if False -- TODO: FIXME
                          then CreateCheckout
                          else CheckoutSucceeded)
                         registrationDetails)))
              if False -- TODO: FIXME:
                 then redirect CheckoutCreateR
                 else redirect CheckoutSuccessR


registerView :: SessionState -> Lucid App () -> Lucid App ()
registerView sessionState formView =
  shopTemplate
    sessionState
    (containedColumn_
       (do url <- ask
           h1_ "Register"
           form_
             [action_ (url EnterDetailsR), method_ "POST", novalidate_ ""] -- TODO: remove novalidate.
             (do formView
                 p_ (button_ [class_ "btn btn-primary"] "Continue"))))

verifiedRegisterForm :: Forge.Default RegistrationDetails -> VerifiedForm RegisterError RegistrationDetails
verifiedRegisterForm = $$($$(Forge.verify1 [||registerForm||]))

--------------------------------------------------------------------------------
-- Checkout create page

getCheckoutCreateR :: Handler (Html ())
getCheckoutCreateR = withRegistrationState _CreateCheckout go
  where
    go state sessionId registrationDetails = do
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
            , customerEmail = unEmail (registerEmail registrationDetails)
            , trialFromPlan = True
            }
      case result of
        Left err -> error (show err) -- TODO:
        Right CreateSessionResponse {checkoutSessionId} -> do
          runDB
            (updateSession
               sessionId
               (Unregistered (WaitingForStripe registrationDetails)))
          htmlWithUrl
            (shopTemplate
               state
               (containedColumn_
                (do script_ [src_ "https://js.stripe.com/v3/"] ("" :: Text)
                    h1_ "Redirecting you to Stripe"
                    noscript_
                      "Please enable JavaScript so that we can securely send you to Stripe."
                    p_ "Redirecting you to Stripe ..."
                    spinner_
                    if False -- TODO: Remove or make debug/release.
                      then redirect_ 3 CheckoutWaitingR
                      else julius_
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
  // using `result.error.message`. -- TODO: display this to the user.
  console.log(result);
});
}, 1000);
|]

--------------------------------------------------------------------------------
-- Checkout waiting page

getCheckoutWaitingR :: Handler (Html ())
getCheckoutWaitingR = withRegistrationState _WaitingForStripe go
  where
    go state sessionId registrationDetails = do
      runDB -- TODO: Actually check Stripe confirmation.
        (updateSession
           sessionId
           (Unregistered (CheckoutSucceeded registrationDetails)))
      htmlWithUrl
        (shopTemplate
           state -- TODO: Only redirect on stripe confirmation.
           (containedColumn_
              (do redirect_ 5 CheckoutSuccessR
                  h1_ "Waiting for Stripe"
                  p_ "We're waiting for the Stripe payment service to tell us whether payment succeeded..."
                  spinner_)))

--------------------------------------------------------------------------------
-- Checkout cancel page

getCheckoutCancelR :: Handler (Html ())
getCheckoutCancelR = withRegistrationState _WaitingForStripe go
  where
    go state sessionId registrationDetails = do
      runDB
        (updateSession
           sessionId
           (Unregistered (EnterDetails (pure registrationDetails))))
      htmlWithUrl
        (shopTemplate
           state
           (do h1_ "Checkout cancelled"
               p_ "Going back to registration..."
               spinner_
               redirect_ 3 EnterDetailsR))

--------------------------------------------------------------------------------
-- Checkout success page

getCheckoutSuccessR :: Handler (Html ())
getCheckoutSuccessR = withRegistrationState _CheckoutSucceeded go
  where
    go state sessionId RegistrationDetails {..} = do
      _ <-
        runDB
              -- TODO: This check needs to happen sooner rather than later.
          (do entity <-
                insertBy
                  Account
                    { accountUsername = registerUsername
                    , accountPassword = registerPassword
                    , accountEmail = registerEmail
                    }
              updateSession
                sessionId
                (Registered
                   LoginState
                     { loginEmail = registerEmail
                     , loginUsername = registerUsername
                     , loginAccountId =
                         fromAccountId (either entityKey id entity)
                     }))
      htmlWithUrl
        (shopTemplate
           state
           (containedColumn_
            (do h1_ "Checkout succeeded!"
                p_ "Great success!"
                p_ "Going to the dashboard..."
                spinner_
                redirect_ 2 AppDashboardR)))
