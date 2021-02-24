{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}

-- | Stripe portal.

module Inflex.Server.Handlers.Portal where

import Data.Coerce
import Inflex.Server.App
import Inflex.Server.Session
import Inflex.Server.Types
import Network.HTTP.Types.Status
import Stripe
import Yesod hiding (Html, lookupSession, deleteSession)

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
         createPortal stripeConfig (coerce accountCustomerId) (render AppDashboardR)
       case result of
         Left {} -> error "Couldn't create a portal with Stripe... maybe try again later?"
         Right CreatePortalResponse {url} -> redirectWith seeOther303 url)
