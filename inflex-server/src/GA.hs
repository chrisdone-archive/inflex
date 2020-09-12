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
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
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
submitGA = when False (do
   liftIO (S8.putStrLn "Should GA")
   mroute <- getCurrentRoute
   lock <- fmap appGALock getYesod
   Config{gaUa} <- fmap appConfig getYesod
   uid <- makeGaUid
   ip <- lookupHeader "x-real-ip"
   mua <- lookupHeader "user-agent"
   mref <- lookupHeader "referer"
   maybe (pure ()) (liftIO . void . forkIO . flip (send mref mua ip uid gaUa) lock) mroute)

-- | Send to GA.
send :: Maybe ByteString -> Maybe ByteString ->  Maybe ByteString ->   GA_UID -> GA_UA -> Route App -> MVar () -> IO ()
send mref mua ip uid gaUa route = flip withMVar (const (sendIt mref mua ip uid gaUa route))

sendIt :: Maybe ByteString ->  Maybe ByteString ->  Maybe ByteString ->   GA_UID ->  GA_UA -> Route App -> IO ()
sendIt mref mua mip uid gaUa route = do
  S8.putStrLn "Queue to send GA."
  case renderRouteForGA route of
    Just rendered -> do
      request <- fmap hydrate (parseRequest endpoint)
      response <- httpLbs request
      L8.putStrLn (getResponseBody response)
      print params
      S8.putStrLn "Sent GA!"
      pure ()
      where params =
              [ ("v", "1")
              , ("t", "pageview")
              , ("tid", T.encodeUtf8 (unGA_UA gaUa))
              , ("dp", T.encodeUtf8 rendered)
              , ("uid", sha256AsHexBS (unGA_UID uid))
              , ("ds", "web")
              , ("dh", "inflex.io")
              , ("aip", "1")
              , ("npa", "1")
              ] <>
              [("uip", ip) | Just ip <- [mip]] <>
              [("ua", ua) | Just ua <- [mua]] <>
              [("dr", dr) | Just dr <- [mref]]
            hydrate =
              setRequestMethod "POST" .
              (case mua of
                 Just ua -> addRequestHeader "user-agent" ua
                 Nothing -> id) .
              setRequestSecure True . setRequestBodyURLEncoded params
    Nothing -> pure ()

endpoint :: String
endpoint = "https://www.google-analytics.com/collect"
-- endpoint = "https://www.google-analytics.com/debug/collect"

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
