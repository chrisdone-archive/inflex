{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
-- |

module Inflex.Server.Handlers.Auth where

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
import           Yesod hiding (Html, lookupSession, deleteSession)
import           Yesod.Forge
import           Yesod.Lucid

handleLoginR :: Handler (Html ())
handleLoginR = do
  submission <- generateForm verifiedLoginForm
  case submission of
    NotSubmitted v -> htmlWithUrl (loginView NoSessionState v)
    Submitted parse -> do
      Forge.Generated {generatedView = v, generatedValue = generatedResult} <-
        runDB parse
      case generatedResult of
        Failure _errors -> htmlWithUrl (loginView NoSessionState v)
        Success (Entity key account) -> do
          _ <-
            assumeSession
              (Registered
                 LoginState
                   { loginEmail = accountEmail account
                   , loginUsername = accountUsername account
                   , loginAccountId = fromAccountId key
                   })
          redirect AppDashboardR

verifiedLoginForm :: VerifiedForm LoginError (Entity Account)
verifiedLoginForm = $$($$(Forge.verify [||loginForm||]))

loginView :: SessionState -> Lucid App () -> Lucid App ()
loginView state formView =
  shopTemplate
    state
    (containedColumn_
     (do h1_ "Login"
         form_
           [method_ "POST"]
           (do formView
               p_ (button_ "Continue"))))

postLogoutR :: Handler ()
postLogoutR = do
  submitGA
  session <- lookupSession
  case session of
    Just (Entity sessionId Session {sessionState = Registered {}}) ->
      do runDB (deleteSession sessionId)
         redirect HomeR
    _ -> redirect HomeR
