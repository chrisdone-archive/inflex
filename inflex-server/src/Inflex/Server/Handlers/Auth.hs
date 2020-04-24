{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
-- |

module Inflex.Server.Handlers.Auth where

import           Data.Functor.Identity
import           Data.Validation
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Server.App
import           Inflex.Server.Forge
import           Inflex.Server.Lucid
import           Inflex.Server.Forms
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.Shop
import           Lucid
import           Yesod hiding (Html, lookupSession, deleteSession)
import           Yesod.Forge
import           Yesod.Lucid

handleLoginR :: Handler (Html ())
handleLoginR = do
  session <- assumeSession NoSessionState
  let state = sessionState (entityVal session)
      sessionId = entityKey session
  submission <- generateForm verifiedLoginForm
  case submission of
    NotSubmitted v -> htmlWithUrl (loginView state v)
    Submitted parse -> do
      let Forge.Generated {generatedView = v, generatedValue = generatedResult} =
            runIdentity parse
      case generatedResult of
        Failure _errors -> htmlWithUrl (loginView state v)
        Success (email, password) -> do
          maccount <-
            runDB
              (selectFirst
                 [AccountEmail ==. email, AccountPassword ==. password]
                 [])
          case maccount of
            Nothing -> htmlWithUrl (loginView state v)
            Just (Entity key account) -> do
              runDB
                (updateSession
                   sessionId
                   (Registered
                      LoginState
                        { loginEmail = email
                        , loginUsername = accountUsername account
                        , loginAccountId = fromAccountId key
                        }))
              redirect AppDashboardR

verifiedLoginForm :: VerifiedForm Error (Email, Password)
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
  session <- lookupSession
  case session of
    Just (Entity sessionId Session {sessionState = Registered {}}) ->
      do runDB (deleteSession sessionId)
         redirect HomeR
    _ -> redirect HomeR
