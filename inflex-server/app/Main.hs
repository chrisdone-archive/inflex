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
import           Criterion.Measurement
import qualified Data.ByteString.Char8 as S8
import           Data.Functor.Contravariant
import           Data.Pool
import           Data.String
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Yaml
import           GitInfo (gitHash)
import           Inflex.Backend
import           Inflex.Migrate
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
import qualified System.Metrics.Prometheus.Concurrent.Registry as Prometheus.Registry
import qualified System.Metrics.Prometheus.Http.Scrape as Prometheus
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import           Yesod hiding (Html)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  Buffering.setAppBuffering
  initializeTime
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
  runRIO
    databaseLogFunc
    (withDBPool
       config
       (\pool -> do
          runSqlPool
            (do when False (printMigration migrateAll) -- Enable when updating the DB.
                manualMigration migrateAll)
            pool
          logFunc <- liftIO (makeAppLogFunc registry)
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

makeAppLogFunc :: Prometheus.Registry.Registry -> IO (GLogFunc AppMsg)
makeAppLogFunc registry = do
  let bucketsSeconds = [ms / 1000 | ms <- [5,10,20,30,40,50,60,70,80,90]]
  documentLoaded <-
    Prometheus.Registry.registerCounter "inflex_DocumentLoaded" mempty registry
  documentLoadedMS <-
    Prometheus.Registry.registerHistogram
      "inflex_DocumentLoadedMS"
      mempty
      bucketsSeconds
      registry
  timeoutExceeded <-
    Prometheus.Registry.registerCounter "inflex_TimeoutExceeded" mempty registry
  documentRefreshed <-
    Prometheus.Registry.registerCounter
      "inflex_DocumentRefreshed"
      mempty
      registry
  documentRefreshedMS <-
    Prometheus.Registry.registerHistogram
      "inflex_DocumentRefreshedMS"
      mempty
      bucketsSeconds
      registry
  updateTransformError <-
    Prometheus.Registry.registerCounter
      "inflex_UpdateTransformError"
      mempty
      registry
  cellUpdated <-
    Prometheus.Registry.registerCounter "inflex_CellUpdated" mempty registry
  cellUpdatedMS <-
    Prometheus.Registry.registerHistogram
      "inflex_CellUpdatedMS"
      mempty
      bucketsSeconds
      registry
  cellErrorInNestedPlace <-
    Prometheus.Registry.registerCounter
      "inflex_CellErrorInNestedPlace"
      mempty
      registry
  openDocument <-
    Prometheus.Registry.registerCounter "inflex_OpenDocument" mempty registry
  createDocument <-
    Prometheus.Registry.registerCounter "inflex_CreateDocument" mempty registry
  deleteDocument <-
    Prometheus.Registry.registerCounter "inflex_DeleteDocument" mempty registry
  renameDocument <-
    Prometheus.Registry.registerCounter "inflex_RenameDocument" mempty registry
  pure
    (mkGLogFunc
       (\_callStack ->
          \case
            ServerMsg msg ->
              case msg of
                DocumentLoaded ms -> do
                  Histogram.observe ms documentLoadedMS
                  Counter.inc documentLoaded
                TimeoutExceeded -> Counter.inc timeoutExceeded
                DocumentRefreshed ms -> do
                  Histogram.observe ms documentRefreshedMS
                  Counter.inc documentRefreshed
                UpdateTransformError -> Counter.inc updateTransformError
                CellUpdated ms -> do
                  Histogram.observe ms cellUpdatedMS
                  Counter.inc cellUpdated
                CellErrorInNestedPlace -> Counter.inc cellErrorInNestedPlace
                OpenDocument -> Counter.inc openDocument
                CreateDocument -> Counter.inc createDocument
                DeleteDocument -> Counter.inc deleteDocument
                RenameDocument -> Counter.inc renameDocument
                StripeCreateCustomerFailed{} -> pure ()
            YesodMsg msg -> when False (prettyWrite msg)
            DatabaseMsg msg -> when False (prettyWrite msg)
            AppWaiMsg msg -> when False (prettyWrite msg)))

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
