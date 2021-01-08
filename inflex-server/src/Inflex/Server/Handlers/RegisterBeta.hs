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

module Inflex.Server.Handlers.RegisterBeta
  ( handleEnterDetailsR
  ) where

import           Control.Monad.Reader
import           Data.Foldable
import           Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import           Data.Validation
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           GA
import           Inflex.Server.App
import           Inflex.Server.Forge
import           Inflex.Server.Forms
import           Inflex.Server.Lucid
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.Shop
import           Lucid
import           Optics
import           Yesod hiding (Html, Field, toHtml)
import           Yesod.Forge
import           Yesod.Lucid

intro_ :: Lucid App ()
intro_ = do
  h1_ "Create a username and password"
  p_
    (do "Don't forget, you can also join the community forum at "
        a_ [href_ "https://community.inflex.io/"] "community.inflex.io"
        " to share your (much needed) feedback!.")

handleEnterDetailsR :: Handler (Html ())
handleEnterDetailsR = withRegistrationState _BetaEnterDetails go
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
            Failure errors ->
              htmlWithUrl
                (do registerView
                      state
                      (do div_
                            [class_ "invalid-feedback"]
                            (traverse_ showError errors)
                          v))
            Success RegistrationDetails {..} -> do
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
                             , accountCustomerId = Nothing
                             }
                       copySampleDocuments key
                       updateSession
                         sessionId
                         (Registered
                            LoginState
                              { loginEmail = registerEmail
                              , loginUsername = Nothing
                              , loginAccountId = fromAccountId key
                              })))
              htmlWithUrl
                (shopTemplate
                   state
                   (div_
                      [class_ "register-page"]
                      (do h1_ "Registered!"
                          p_ "Taking you to the dashboard..."
                          redirect_ 3 AppDashboardR)))

copySampleDocuments :: AccountId -> YesodDB App ()
copySampleDocuments targetAccountId = do
  maccount <- selectFirst [AccountEmail ==. Email "templates@inflex.io"] [] -- TODO: put in config file
  let maccountId = fmap entityKey maccount
  case maccountId of
    Nothing -> pure ()
    Just sourceAccountId -> do
      oldDocuments <-
        selectList [DocumentAccount ==. sourceAccountId] [Desc DocumentId]
      let oldKeys = map entityKey oldDocuments
      now <- liftIO getCurrentTime
      newKeys <-
        insertMany
          (map
             (\(Entity _ Document {..}) ->
                Document
                  { documentAccount = targetAccountId
                  , documentName
                  , documentUpdated = now
                  , documentCreated = now
                  })
             oldDocuments)
      oldRevisions <-
        selectList
          [ RevisionAccount ==. sourceAccountId
          , RevisionDocument <-. oldKeys
          , RevisionActive ==. True
          ]
          [Desc RevisionDocument]
      insertMany_
        (zipWith
           (\newKey Revision {..} ->
              Revision
                { revisionContent
                , revisionActive = True
                , revisionDocument = newKey
                , revisionCreated = now
                , revisionActivated = now
                , revisionAccount = targetAccountId
                })
           newKeys
           (map entityVal oldRevisions))
      pure ()

registerView :: SessionState -> Lucid App () -> Lucid App ()
registerView sessionState formView =
  shopTemplate
    sessionState
    (div_
       [class_ "register-page"]
       (do url <- ask
           form_
             [action_ (url EnterDetailsR), method_ "POST"]
             (do intro_
                 formView
                 p_ (button_ [class_ "btn btn-primary"] "Create and go to dashboard"))))

verifiedRegisterForm :: Forge.Default RegistrationDetails -> VerifiedForm RegisterError RegistrationDetails
verifiedRegisterForm = $$($$(Forge.verify1 [||registerFormBeta||]))

--------------------------------------------------------------------------------
-- State to page mapping
--
-- This is copied from the real stripe-based registration, for the
-- purpose of the beta.
--
-- There's currently only one beta registration state (entering
-- details), but there may be others in the future!

registerRedirect :: BetaRegistrationState -> Handler (Html ())
registerRedirect state =
  case state of
    BetaEnterDetails {} -> redirect' EnterDetailsR
  where
    redirect' route =
      htmlWithUrl -- TODO: Fix the below for real-world use.
        (shopTemplate
           (UnregisteredBeta state)
           (do _url <- ask
               h1_
                 (do "Wrong page!"
                     noscript_ (code_ (toHtml (show state))))
               p_ "Taking you to the right one ..."
               redirect_ 2 route))

withRegistrationState ::
     Iso' BetaRegistrationState a
  -> (SessionState -> SessionId -> a -> Handler (Html ()))
  -> Handler (Html ())
withRegistrationState theCons cont = do
  submitGA
  session <- assumeSession registerStart
  case session of
    (Entity sessionId Session {sessionState = state}) ->
      case state of
        UnregisteredBeta registerState ->
          case preview theCons registerState of
            Nothing -> registerRedirect registerState
            Just a -> cont state sessionId a
        Registered {} ->
          htmlWithUrl
            (shopTemplate
               state
               (do h1_ "Registered!"
                   p_
                     "You are registered! Taking you to the dashboard..."
                   spinner_
                   redirect_ 2 AppDashboardR))
        -- The real registration just starts a-fresh.
        Unregistered {} -> do
          runDB (updateSession sessionId registerStart)
          redirect EnterDetailsR
        NoSessionState {} -> do
          runDB (updateSession sessionId registerStart)
          redirect EnterDetailsR
  where
    registerStart = UnregisteredBeta (BetaEnterDetails Nothing)
