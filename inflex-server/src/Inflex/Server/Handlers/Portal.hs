{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}

-- | Stripe portal.

module Inflex.Server.Handlers.Portal where

import           Data.Coerce
import           Data.Functor
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Network.HTTP.Types.Status
import           Stripe
import           Yesod hiding (Html, lookupSession, deleteSession)

-- | Create a portal and send the user through it.
postPortalR :: Handler ()
postPortalR = do
  withLogin
    (\_ LoginState {loginAccountId} -> do
       Account {accountCustomerId} <-
         runDB (get404 (fromAccountID loginAccountId))
       render <- getUrlRender
       Config {stripeConfig} <- fmap appConfig getYesod
       result <-
         createPortal stripeConfig (coerce accountCustomerId) (render AccountR)
       case result of
         Left {} -> error "Couldn't create a portal with Stripe... maybe try again later?"
         Right CreatePortalResponse {url} -> redirectWith seeOther303 url)

getCheckoutSessionCompletedR :: NonceUUID -> CustomerId -> HandlerFor App ()
getCheckoutSessionCompletedR nonceUUID customerId = do
  msession <- runDB (queryNonceSession nonceUUID)
  case msession of
    Nothing -> error "Invalid session -- do not retry."
    Just (Entity sessionId Session {sessionState}) ->
      case sessionState of
        NoSessionState {} -> error "Session has no state -- do not retry."
        Registered loginState@LoginState {loginAccountId} ->
          runDB
            (update (fromAccountID loginAccountId) [AccountSubscribed =. True])
        UnregisteredBeta {} -> error "UnregisteredBeta is wrong here."
        Unregistered unregisteredState ->
          case unregisteredState of
            WaitingForStripe RegistrationDetails {..} ->
              void
                (runDB
                   (do resetSessionNonce sessionId
                       salt <-
                         fmap (Salt . UUID.toText) (liftIO UUID.nextRandom)
                       key <-
                         insert
                           Account
                             { accountUsername = Nothing
                             , accountPassword =
                                 sha256Password salt registerPassword
                             , accountSalt = salt
                             , accountEmail = registerEmail
                             , accountCustomerId = customerId
                             , accountSubscribed = True
                             }
                       updateSession
                         sessionId
                         (Registered
                            LoginState
                              { loginEmail = registerEmail
                              , loginUsername = Nothing
                              , loginAccountId = fromAccountId key
                              , loginCustomerId = customerId
                              })))
            _ ->
              error
                ("Not expecting stripe at this point: " <>
                 show unregisteredState)
