{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Session management.

module Inflex.Server.Session
  ( updateSession
  , requireSession
  , lookupSession
  , generateSession
  , assumeSession
  ) where

import qualified Data.Text.Encoding as T
import           Data.UUID as UUID
import           Data.UUID.V4 as UUID
import           Inflex.Server.App
import           Inflex.Server.Types
import           Web.Cookie
import           Yesod hiding (lookupSession)

assumeSession :: SessionState -> Handler (Entity Session)
assumeSession sessionState = do
  result <- lookupSession
  case result of
    Nothing -> do
      session <- runDB (generateSession sessionState)
      -- TODO: Set expiry, secure, etc.
      -- <https://hackage.haskell.org/package/cookie-0.4.0/docs/Web-Cookie.html#t:SetCookie>
      setCookie
        (def
           { setCookieName = "SESSION-UUID"
           , setCookieValue =
               T.encodeUtf8 (UUID.toText (unSessionUUID (sessionUuid (entityVal session))))
           })
      pure session
    Just session -> pure session

updateSession :: Entity Session -> YesodDB App ()
updateSession (Entity sessionId session) =
  update sessionId [SessionState =. sessionState session]

requireSession :: Route App -> Handler (Entity Session)
requireSession route = do
  result <- lookupSession
  case result of
    Nothing -> redirect route
    Just session -> pure session

lookupSession :: Handler (Maybe (Entity Session))
lookupSession = do
  result <- lookupCookie "SESSION-UUID"
  case result >>= UUID.fromText of
    Just sessionUUID -> runDB (querySession (SessionUUID sessionUUID))
    Nothing -> pure Nothing

querySession :: SessionUUID -> YesodDB App (Maybe (Entity Session))
querySession sessionUuid = selectFirst [SessionUuid ==. sessionUuid] []

generateSession :: SessionState -> YesodDB App (Entity Session)
generateSession sessionState = loop
  where
    loop = do
      uuid <- liftIO UUID.nextRandom
      let sessionUuid = SessionUUID uuid
          session = Session {sessionUuid, sessionState = sessionState}
      result <- insertUnique session
      case result of
        Nothing -> loop
        Just sessionId -> pure (Entity sessionId session)
