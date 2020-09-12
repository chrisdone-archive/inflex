{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Google Analytics.
--
-- <https://ga-dev-tools.appspot.com/hit-builder/>
-- <https://developers.google.com/analytics/devguides/collection/protocol/v1/reference>
-- <https://developers.google.com/analytics/devguides/collection/protocol/v1/parameters>

module GA (submitGA) where

import           Control.Concurrent
import qualified Data.ByteString.Char8 as S8
import           Data.Functor
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Inflex.Server.App
import           Inflex.Server.Types
import           Inflex.Server.Types.Sha256
import           Network.HTTP.Simple
import           Yesod

-- | Submit the current page to GA.
submitGA :: Handler ()
submitGA = do
  liftIO (S8.putStrLn "Should GA")
  mroute <- getCurrentRoute
  lock <- fmap appGALock getYesod
  Config{gaUa} <- fmap appConfig getYesod
  uid <- makeGaUid
  maybe (pure ()) (liftIO . void . forkIO . flip (send uid gaUa) lock) mroute

-- | Send to GA.
send :: GA_UID -> GA_UA -> Route App -> MVar () -> IO ()
send uid gaUa route = flip withMVar (const (sendIt uid gaUa route))

sendIt :: GA_UID ->  GA_UA -> Route App -> IO ()
sendIt uid gaUa route = do
  S8.putStrLn "Queue to send GA."
  case renderRouteForGA route of
    Just rendered -> do
      request <- fmap hydrate (parseRequest endpoint)
      _response <- httpNoBody request
      S8.putStrLn "Sent GA!"
      pure ()
      where hydrate =
              setRequestMethod "POST" .
              setRequestSecure True .
              setRequestBodyURLEncoded
                [ ("v", "1")
                , ("t", "pageview")
                , ("tid", T.encodeUtf8 (unGA_UA gaUa))
                , ("dp", T.encodeUtf8 rendered)
                , ("uid", sha256AsHexBS (unGA_UID uid))
                , ("ds", "web")
                , ("dh", "inflex.io")
                ]
    Nothing -> pure ()

endpoint :: String
endpoint = "https://www.google-analytics.com/collect"

-- | Render a route for consumption by GA -- we avoid sending
-- PII/personal info.
renderRouteForGA :: Route App -> Maybe Text
renderRouteForGA =
  \case
    HomeR -> pure "/"
    EnterDetailsR -> pure "/register/enter-details"
    CheckoutCreateR -> pure "/register/checkout/create"
    CheckoutCancelR -> pure "/register/checkout/cancel"
    CheckoutWaitingR -> pure "/register/checkout/waiting"
    LoginR -> pure "/login"
    LogoutR -> pure "/logout"
    AppDashboardR -> pure "/dashboard"
    NewDocumentR -> pure "/dashboard/new"
    AppEditorR{} -> pure "/editor"
    _ -> Nothing

makeGaUid :: Handler GA_UID
makeGaUid = do
  userAgent <- lookupHeader "user-agent"
  ip <- lookupHeader "X-Real-IP"
  pure (GA_UID (sha256ByteString (fromMaybe "" ip <> fromMaybe "" userAgent)))
