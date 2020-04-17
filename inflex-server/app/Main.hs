{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as S8
import           Data.Pool
import qualified Data.Text.Encoding as T
import           Data.Yaml
import           Inflex.Backend
import           Inflex.Server.App
import           Inflex.Server.Dispatch ()
import           Inflex.Server.Types
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           System.Environment
import           Yesod hiding (Html)

--------------------------------------------------------------------------------
-- Main entry point

-- TODO: RIO

main :: IO ()
main = do
  fp <- getEnv "CONFIG"
  config <- decodeFileEither fp >>= either throwIO return
  port <- fmap read (getEnv "PORT")
  withDBPool config
    (\pool -> do
       withResource
         pool
         (runReaderT
            (do runMigration migrateAll
                manualMigration))
       app <- liftIO (toWaiAppPlain App {appPool = pool, appConfig = config})
       -- Not important for local dev, but will be important when deploying online.
       -- TODO: Middleware for password-protecting the site
       --    <https://hackage.haskell.org/package/wai-extra-3.0.29.1/docs/Network-Wai-Middleware-HttpAuth.html>
       -- TODO: Middleware for blocking non-load-balancer requests
       --   remoteHost = 109.175.148.125:56616,
       --     vs
       --   remoteHost = 10.106.0.4:39350,
       -- TODO: Middleware for blocking abusive connections?
       liftIO (run port (gzip def {gzipFiles = GzipCompress} app)))

withDBPool ::
     (IsPersistBackend b, BaseBackend b ~ SqlBackend)
  => Config -> (Pool b -> LoggingT IO a)
  -> IO a
withDBPool config cont = do
  runStdoutLoggingT
    (filterLogger
       (\_src _lvl -> False)
       (withBackendPool (T.encodeUtf8 (databaseConn config)) 10 cont))
