{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import qualified Buffering
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.ByteString.Char8 as S8
import           Data.Functor.Contravariant
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
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.Gzip
import           RIO
import           RIO.Warp
import           System.Environment
import           Yesod hiding (Html)

import qualified System.Metrics.Prometheus.Concurrent.Registry as Prometheus.Registry
import qualified System.Metrics.Prometheus.Http.Scrape as Prometheus
import qualified System.Metrics.Prometheus.Metric.Counter as Counter

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  Buffering.setAppBuffering
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
  connectCounter <-
    Prometheus.Registry.registerCounter
      "example_connection_total"
      mempty
      registry
  Counter.inc connectCounter
  runRIO
    databaseLogFunc
    (withDBPool
       config
       (\pool -> do
          runSqlPool
            (do when False (printMigration migrateAll) -- Enable when updating the DB.
                manualMigration migrateAll)
            pool
          let logFunc = makeAppLogFunc
          app <-
            liftIO
              (toWaiAppPlain
                 App {appLogFunc = logFunc, appPool = pool, appConfig = config})
          let runMyWarp thisPort =
                Warp.runSettings (Warp.setPort thisPort (warpSettings logFunc))
          liftIO
            (concurrently_
               (runMyWarp
                  9090
                  (Prometheus.prometheusApp
                     ["metrics"]
                     (Prometheus.Registry.sample registry)))
               (runMyWarp
                  port
                  (addServerHeader (gzip def {gzipFiles = GzipCompress} app))))))
  where
    warpSettings parentGLogFunc =
      Warp.setLogger
        (waiLogger (contramap AppWaiMsg parentGLogFunc))
        Warp.defaultSettings

withDBPool ::
     ( HasGLogFunc e
     , GMsg e ~ DatabaseLog
     , IsPersistBackend b
     , BaseBackend b ~ SqlBackend
     )
  => Config
  -> (Pool b -> RIO e a)
  -> RIO e a
withDBPool config cont = do
  e <- ask
  liftIO
    (runLoggingT
       (withBackendPool (T.encodeUtf8 (databaseConn config)) 10 (runRIO e . cont))
       (\loc lsrc llevel lstr ->
          runReaderT (glog (DatabaseLog loc lsrc llevel lstr)) e))

makeAppLogFunc :: GLogFunc AppMsg
makeAppLogFunc =
  mkGLogFunc
    (\_callStack ->
       \case
         YesodMsg msg -> when True (prettyWrite msg)
         DatabaseMsg msg -> when True (prettyWrite msg)
         AppWaiMsg msg -> when False (prettyWrite msg))

prettyWrite :: Show a => a -> IO ()
prettyWrite x = do
  now <- getCurrentTime
  write now x
  where
    write now = S8.putStrLn . S8.pack . ((show now <> " ") <>) . show

databaseLogFunc :: GLogFunc DatabaseLog
databaseLogFunc =
  mkGLogFunc
    (\_callStack ->
       \case
         msg -> when False (prettyWrite msg))
