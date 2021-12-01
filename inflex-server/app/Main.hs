{-# LANGUAGE TupleSections #-}
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
import qualified Data.Map.Strict as M
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
import           System.Mem
import qualified System.Metrics.Prometheus.Concurrent.Registry as Prometheus.Registry
import qualified System.Metrics.Prometheus.Http.Scrape as Prometheus
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import           System.Posix.Signals
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
                            (do RIO.runRIO logFunc (glog (ServerMsg ResettingCache))
                                RIO.threadDelay (1000 * 1000 * 60 * 5)
                                writeIORef loadedRef mempty
                                writeIORef evaledRef mempty
                                performMajorGC))
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
  majorgcsCounter <- Prometheus.Registry.registerCounter "rts_major_gcs" mempty registry
  allocated_bytesCounter <-
    Prometheus.Registry.registerCounter "rts_allocated_bytes" mempty registry
  max_live_bytesCounter <-
    Prometheus.Registry.registerGauge "rts_max_live_bytes" mempty registry
  live_bytesCounter <-
    Prometheus.Registry.registerGauge "rts_live_bytes" mempty registry
  max_mem_in_use_bytesGauge <-
    Prometheus.Registry.registerGauge "rts_max_mem_in_use_bytes" mempty registry
  mem_in_use_bytesGauge <-
    Prometheus.Registry.registerGauge "rts_mem_in_use_bytes" mempty registry
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
             (do performMinorGC
                 stats <- getRTSStats
                 Counter.set (fromIntegral (gcs stats)) gcsCounter
                 Counter.set (fromIntegral (major_gcs stats)) majorgcsCounter
                 Counter.set
                   (fromIntegral (allocated_bytes stats))
                   allocated_bytesCounter
                 Gauge.set
                   (fromIntegral (max_mem_in_use_bytes stats))
                   max_mem_in_use_bytesGauge
                 Gauge.set
                   (fromIntegral (gcdetails_mem_in_use_bytes (gc stats)))
                   mem_in_use_bytesGauge
                 Gauge.set
                   (fromIntegral (max_live_bytes stats))
                   max_live_bytesCounter
                 Gauge.set
                   (fromIntegral (gcdetails_live_bytes (gc stats)))
                   live_bytesCounter
                 Counter.set
                   (fromIntegral (gc_elapsed_ns stats))
                   gc_elapsed_nsCounter
                 Counter.set (fromIntegral (elapsed_ns stats)) elapsed_nsCounter
                 RIO.threadDelay (1000 * 1000 * 60)))))
  timeoutExceeded <-
    Prometheus.Registry.registerCounter
      "inflex_TimeoutExceeded_total"
      mempty
      registry
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
      "inflex_UpdateTransformError_total"
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
      "inflex_CellErrorInNestedPlace_total"
      mempty
      registry
  openDocument <-
    Prometheus.Registry.registerCounter
      "inflex_OpenDocument_total"
      mempty
      registry
  createDocument <-
    Prometheus.Registry.registerCounter
      "inflex_CreateDocument_total"
      mempty
      registry
  deleteDocument <-
    Prometheus.Registry.registerCounter
      "inflex_DeleteDocument_total"
      mempty
      registry
  renameDocument <-
    Prometheus.Registry.registerCounter
      "inflex_RenameDocument_total"
      mempty
      registry
  resettingCache <-
    Prometheus.Registry.registerCounter
      "inflex_ResettingCache_total"
      mempty
      registry
  visitCounters <-
    fmap M.fromList $
    forM
      [minBound .. maxBound]
      (\visitType ->
         fmap
           (visitType, )
           (Prometheus.Registry.registerCounter
              (fromString ("inflex_AnalyticsMsg_" ++ show visitType ++ "_total"))
              mempty
              registry))
  pure
    (mkGLogFunc
       (\_callStack ->
          \case
            ServerMsg msg -> do
              case msg of
                CellSharedResult {} -> pure ()
                _ -> prettyWrite msg
              case msg of
                TimeoutExceeded {} -> Counter.inc timeoutExceeded
                UpdateTransformError -> Counter.inc updateTransformError
                CellErrorInNestedPlace -> Counter.inc cellErrorInNestedPlace
                OpenDocument -> Counter.inc openDocument
                CreateDocument -> Counter.inc createDocument
                DeleteDocument -> Counter.inc deleteDocument
                RenameDocument -> Counter.inc renameDocument
                ResettingCache -> Counter.inc resettingCache
                StripeCreateCustomerFailed {} -> pure ()
                SubscriptionUpdated {} -> pure ()
                CellResultOk {} -> pure ()
                AnalyticsMsg visit ->
                  case M.lookup visit visitCounters of
                    Nothing -> pure ()
                    Just counter -> Counter.inc counter
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
