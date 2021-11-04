{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import qualified Buffering
import           Control.Concurrent
import           Control.Concurrent (killThread)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Criterion.Measurement
import qualified Data.ByteString.Char8 as S8
import           Data.Functor.Contravariant
import qualified Data.List as List
import           Data.Pool
import           Data.String
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Yaml
import           GHC.Stats
import           GitInfo (gitHash)
import           Inflex.Backend
import           Inflex.Migrate
import           Inflex.Server.App
import           Inflex.Server.Dispatch ()
import           Inflex.Server.Types
import           Log
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
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import           System.Posix.Signals
-- import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import           Yesod hiding (Html)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  mainId <- RIO.myThreadId
  _ <-
    installHandler
      softwareTermination
      (CatchOnce
         (do S8.putStrLn "Received SIGTERM. Killing main thread."
             killThread mainId))
      Nothing
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
  config <- decodeFileEither fp >>= either RIO.throwIO return
  port <- fmap read (getEnv "PORT")
  registry <- Prometheus.Registry.new
  finally
    (runRIO
       databaseLogFunc
       (withDBPool
          config
          (\pool -> do
             runSqlPool (manualMigration (stripeConfig config) migrateAll) pool
             entries <- runSqlPool (showMigration migrateAll) pool
             if not (List.null (filter (flip notElem skipPersistent) entries))
               then liftIO
                      (do putStrLn
                            "Persistent has an inconsistent view of the database."
                          putStrLn "Persistent's proposed changes:"
                          mapM_ T.putStrLn entries
                          exitFailure)
               else do
                 let skipped = filter (flip elem skipPersistent) entries
                 when
                   (not (null skipped))
                   (liftIO
                      (putStrLn
                         ("Skipped " ++
                          show (length skipped) ++ " persistent suggestions.")))
                 logFunc <- liftIO (makeAppLogFunc registry)
                 loadedRef <- newIORef mempty
                 evaledRef <- newIORef mempty
                 app <-
                   liftIO
                     (toWaiAppPlain
                        App
                          { appLogFunc = logFunc
                          , appPool = pool
                          , appConfig = config
                          , appLoadCache = loadedRef
                          , appEvalCache = evaledRef
                          , appStatic = app_static
                          })
                 let runMyWarp thisPort =
                       Warp.runSettings
                         (Warp.setPort thisPort (warpSettings logFunc))
                 liftIO
                   (concurrently_
                      (runMyWarp
                         9090
                         (Prometheus.prometheusApp
                            ["metrics"]
                            (Prometheus.Registry.sample registry)))
                      (concurrently_
                         -- Reset cache every 5 minutes. Arbitrary.
                         (forever
                            (do RIO.threadDelay (1000 * 1000 * 60 * 5)
                                writeIORef loadedRef mempty
                                writeIORef evaledRef mempty))
                         (runMyWarp
                            port
                            (addServerHeader
                               (gzip def {gzipFiles = GzipCompress} app))))))))
    (S8.putStrLn "Server shutdown: OK")
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
makeAppLogFunc registry
  {-let bucketsSeconds =
        [ms / 1000 | ms <- [5, 10, 20, 30, 40, 50, 60, 70, 80, 90]]-}
  -- documentLoaded <-
  --   Prometheus.Registry.registerCounter "inflex_DocumentLoaded" mempty registry
  -- documentLoadedMS <-
  --   Prometheus.Registry.registerHistogram
  --     "inflex_DocumentLoadedMS"
  --     mempty
  --     bucketsSeconds
  --     registry
 = do
  gcsCounter <- Prometheus.Registry.registerCounter "rts_gcs" mempty registry
  allocated_bytesCounter <-
    Prometheus.Registry.registerCounter "rts_allocated_bytes" mempty registry
  max_mem_in_use_bytesGauge <-
    Prometheus.Registry.registerGauge
      "rts_max_mem_in_use_bytes"
      mempty
      registry
  gc_elapsed_nsCounter <-
    Prometheus.Registry.registerCounter "rts_gc_elapsed_ns" mempty registry
  elapsed_nsCounter <-
    Prometheus.Registry.registerCounter "rts_elapsed_ns" mempty registry
  enabled <- getRTSStatsEnabled
  when
    enabled
    (void
       (forkIO
          (forever
             (do stats <- getRTSStats
                 Counter.add (fromIntegral (gcs stats)) gcsCounter
                 Counter.add (fromIntegral (allocated_bytes stats)) allocated_bytesCounter
                 Gauge.set (fromIntegral (max_mem_in_use_bytes stats)) max_mem_in_use_bytesGauge
                 Counter.add (fromIntegral (gc_elapsed_ns stats)) gc_elapsed_nsCounter
                 Counter.add (fromIntegral (elapsed_ns stats)) elapsed_nsCounter
                 RIO.threadDelay (1000 * 1000 * 60)))))
  timeoutExceeded <-
    Prometheus.Registry.registerCounter "inflex_TimeoutExceeded" mempty registry
  -- documentRefreshed <-
  --   Prometheus.Registry.registerCounter
  --     "inflex_DocumentRefreshed"
  --     mempty
  --     registry
  -- documentRefreshedMS <-
  --   Prometheus.Registry.registerHistogram
  --     "inflex_DocumentRefreshedMS"
  --     mempty
  --     bucketsSeconds
  --     registry
  updateTransformError <-
    Prometheus.Registry.registerCounter
      "inflex_UpdateTransformError"
      mempty
      registry
  -- cellUpdated <-
  --   Prometheus.Registry.registerCounter "inflex_CellUpdated" mempty registry
  -- cellUpdatedMS <-
  --   Prometheus.Registry.registerHistogram
  --     "inflex_CellUpdatedMS"
  --     mempty
  --     bucketsSeconds
  --     registry
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
            ServerMsg msg -> do
              prettyWrite msg
              case msg of
                TimeoutExceeded {} -> Counter.inc timeoutExceeded
                UpdateTransformError -> Counter.inc updateTransformError
                CellErrorInNestedPlace -> Counter.inc cellErrorInNestedPlace
                OpenDocument -> Counter.inc openDocument
                CreateDocument -> Counter.inc createDocument
                DeleteDocument -> Counter.inc deleteDocument
                RenameDocument -> Counter.inc renameDocument
                StripeCreateCustomerFailed {} -> pure ()
                SubscriptionUpdated {} -> pure ()
                CellResultOk {} -> pure ()
                Timed {} -> pure ()
                LoadDocumentMsg {} -> pure ()
                CellError {} -> pure ()
                CellHash {} -> pure ()
                CellSharedResult {} -> pure ()
            YesodMsg msg -> when False (prettyWrite msg)
            DatabaseMsg msg -> when False (prettyWrite msg)
            AppWaiMsg msg -> when False (prettyWrite msg)))

databaseLogFunc :: GLogFunc DatabaseLog
databaseLogFunc =
  mkGLogFunc
    (\_callStack ->
       \case
         msg -> when False (prettyWrite msg))
