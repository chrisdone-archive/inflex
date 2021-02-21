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
  ( -- Disabled while we're in beta. Alternative registration is provided by Inflex.Server.Handlers.RegisterBeta.
    -- handleEnterDetailsR
   getCheckoutCreateR
  , getCheckoutCancelR
  , getCheckoutWaitingR
  , getCheckoutSessionCompletedR
  ) where

import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Data.Validation
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           GA
import           Inflex.Server.App
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
  where
    redirect' route =
      htmlWithUrl -- TODO: Fix the below for real-world use.
        (shopTemplate
           (Unregistered state)
           (containedColumn_
              (do _url <- ask
                  h1_
                    (do "Wrong page!"
                        noscript_ (code_ (toHtml (show state))))
                  p_ "Taking you to the right one ..."
                  redirect_ 2 route)))

withRegistrationState ::
     Prism' RegistrationState a
  -> (SessionState -> SessionId -> a -> Handler (Html ()))
  -> Handler (Html ())
withRegistrationState theCons cont = do
  submitGA
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
                  (do h1_ "Registered!"
                      p_
                        "You are registered! Taking you to the dashboard..."
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
                      (CreateCheckout
                         registrationDetails)))
              redirect CheckoutCreateR

registerView :: SessionState -> Lucid App () -> Lucid App ()
registerView sessionState formView =
  shopTemplate
    sessionState
    (containedColumn_
       (do url <- ask
           h1_ "Register"
           form_
             [action_ (url EnterDetailsR), method_ "POST"]
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
      session <- runDB (get404 sessionId)
      render <- getUrlRender
      Config {stripeConfig} <- fmap appConfig getYesod
      result <-
        Stripe.createSession
          StripeSession
            { stripeConfig
            , successUrl = render CheckoutWaitingR
            , cancelUrl = render CheckoutCancelR
            , customerEmail = unEmail (registerEmail registrationDetails)
            , trialFromPlan = True
            , clientReferenceId = maybe "" (UUID.toText . unNonceUUID) (sessionNonce session)
            }
      case result of
        Left err -> error (show err) -- TODO: handle this properly.
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
                    h1_ "Redirecting you to secure payment"
                    noscript_
                      "Please enable JavaScript so that we can securely send you to Stripe."
                    p_ "Redirecting you to Stripe ..."
                    spinner_
                    julius_
                      (stripeJavaScript
                         (publishableApiKey stripeConfig)
                         checkoutSessionId))))

-- TODO: display error messsage to the user (see JS comment below).
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
    go state _sessionId _registrationDetails = do
      htmlWithUrl
        (shopTemplate
           state
           (containedColumn_
              (do refresh_ 5
                  h1_ "Waiting for Stripe"
                  p_ "We're waiting for the Stripe payment service to tell us whether payment succeeded..."
                  spinner_
                  url <- ask
                  p_ (a_ [href_ (url CheckoutCancelR)] "Cancel"))))

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

getCheckoutSessionCompletedR :: NonceUUID -> CustomerId -> HandlerFor App ()
getCheckoutSessionCompletedR nonceUUID customerId = do
  msession <- runDB (queryNonceSession nonceUUID)
  case msession of
    Nothing -> error "Invalid session -- do not retry."
    Just (Entity sessionId Session {sessionState}) ->
      case sessionState of
        NoSessionState {} -> error "Session has no state -- do not retry."
        Registered {} -> error "Already registered. Bug?"
        Unregistered unregisteredState ->
          case unregisteredState of
            WaitingForStripe RegistrationDetails {..} ->
              void
                (runDB
                   (do resetSessionNonce sessionId
                       salt <- fmap (Salt . UUID.toText) (liftIO UUID.nextRandom)
                       key <-
                         insert
                           Account
                             { accountUsername = Nothing
                             , accountPassword = sha256Password salt registerPassword
                             , accountSalt = salt
                             , accountEmail = registerEmail
                             , accountCustomerId = customerId
                             }
                       updateSession
                         sessionId
                         (Registered
                            LoginState
                              { loginEmail = registerEmail
                              , loginUsername = Nothing
                              , loginAccountId =
                                  fromAccountId key
                              })))
            _ ->
              error
                ("Not expecting stripe at this point: " <>
                 show unregisteredState)
