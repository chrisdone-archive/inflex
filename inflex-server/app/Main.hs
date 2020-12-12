{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.Pool
import           Data.String
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Yaml
import           GitInfo (gitHash)
import           Inflex.Backend
import           Inflex.Server.App
import           Inflex.Server.Dispatch ()
import           Inflex.Server.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.Gzip
import           System.Environment
import           Yesod hiding (Html)

import qualified System.Metrics.Prometheus.Concurrent.Registry as Prometheus.Registry
import qualified System.Metrics.Prometheus.Http.Scrape as Prometheus
import qualified System.Metrics.Prometheus.Metric.Counter as Counter

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  now <- getCurrentTime
  let addServerHeader :: Middleware
      addServerHeader =
        addHeaders
          [ ("Server", "inflex-server")
          , ("X-Git-Commit", fromString gitHash)
          , ("X-Process-Started", fromString (show now))
          ]
  fp <- getEnv "CONFIG"
  config <- decodeFileEither fp >>= either throwIO return
  port <- fmap read (getEnv "PORT")
  registry <- Prometheus.Registry.new
  connectCounter <- Prometheus.Registry.registerCounter "example_connection_total" mempty registry
  Counter.inc connectCounter
  withDBPool
    config
    (\pool -> do
       withResource
         pool
         (runReaderT
            (do when False (printMigration migrateAll) -- Enable when updating the DB.
                manualMigration migrateAll))
       app <- liftIO (toWaiApp App {appPool = pool, appConfig = config})
       liftIO
         (concurrently_
            (run
               9090
               (Prometheus.prometheusApp
                  ["metrics"]
                  (Prometheus.Registry.sample registry)))
            (run
               port
               (addServerHeader (gzip def {gzipFiles = GzipCompress} app)))))

withDBPool ::
     (IsPersistBackend b, BaseBackend b ~ SqlBackend)
  => Config -> (Pool b -> LoggingT IO a)
  -> IO a
withDBPool config cont = do
  runStdoutLoggingT
    (filterLogger
       (\_src _lvl -> False)
       (withBackendPool (T.encodeUtf8 (databaseConn config)) 10 cont))
