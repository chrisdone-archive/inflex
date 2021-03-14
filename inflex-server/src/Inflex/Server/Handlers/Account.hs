{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields, OverloadedStrings #-}

-- | Your Inflex account.

module Inflex.Server.Handlers.Account where

import           Control.Monad.Reader
import           Data.Coerce
import qualified Data.UUID as UUID
import           Inflex.Server.App
import           Inflex.Server.Lucid
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.Shop
import           Lucid
import           Stripe
import           Yesod hiding (Html)
import           Yesod.Lucid

-- | Manage account.
getAccountR :: Handler (Html ())
getAccountR = do
  withLogin
    (\_ sessionState@LoginState {loginAccountId} -> do
       account <- runDB (get404 (fromAccountID loginAccountId))
       htmlWithUrl
         (shopTemplate
            (Registered sessionState)
            (do h1_ "Account"
                url <- ask
                let subscribed = accountSubscribed account
                if subscribed
                  then do
                    p_ (strong_ [class_ "subscribed"] "Subscribed")
                  else do
                    p_ (strong_ [class_ "unsubscribed"] "Not Subscribed")
                    form_
                      [action_ (url SubscribeR), method_ "post"]
                      (p_ (button_ [class_ "full-button"] "Subscribe Now"))
                form_
                  [action_ (url PortalR), method_ "post"]
                  (p_
                     (button_
                        [class_ "full-button"]
                        "Manage Subscription and Payment Methods")))))

postSubscribeR :: Handler (Html ())
postSubscribeR =
  withLogin
    (\sessionId loginState@LoginState {loginCustomerId} -> do
       render <- getUrlRender
       Config {stripeConfig} <- fmap appConfig getYesod
       nonce <- runDB (freshSessionNonce sessionId)
       result <-
         Stripe.createSession
           StripeSession
             { stripeConfig
             , successUrl = render ConfirmSubscribeR
             , cancelUrl = render AccountR
             , clientReferenceId = UUID.toText nonce
             , customer = ExistingCustomer (coerce loginCustomerId)
             }
       case result of
         Left err -> error (show err) -- TODO: handle this properly.
         Right CreateSessionResponse {id = checkoutSessionId} -> do
           htmlWithUrl
             (shopTemplate
                (Registered loginState)
                (containedColumn_
                   (do h1_ "Redirecting you to Stripe"
                       noscript_
                         "Please enable JavaScript so that we can securely send you to Stripe."
                       p_ "Redirecting you to Stripe ..."
                       spinner_
                       stripeCheckout_
                         (publishableApiKey stripeConfig)
                         checkoutSessionId))))

getConfirmSubscribeR :: Handler (Html ())
getConfirmSubscribeR = do
  withLogin
    (\_ sessionState@LoginState {loginAccountId} -> do
       account <- runDB (get404 (fromAccountID loginAccountId))
       if accountSubscribed account
         then do
           htmlWithUrl
             (shopTemplate
                (Registered sessionState)
                (do h1_ "Got response from Stripe"
                    redirect_ 2 AccountR
                    p_ "OK, you're subscribed!"
                    spinner_))
         else htmlWithUrl
                (shopTemplate
                   (Registered sessionState)
                   (do h1_ "Waiting for Stripe"
                       refresh_ 2
                       p_
                         "We're waiting for the Stripe service to tell us whether subscription succeeded..."
                       spinner_)))
