{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as S8
import           Data.Pool
import           Inflex.Backend
import           Inflex.Server.App
import           Inflex.Server.Dispatch ()
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip
import           System.Environment
import           Yesod hiding (Html)

--------------------------------------------------------------------------------
-- Main entry point

-- TODO: RIO

main :: IO ()
main = do
  port <- fmap read (getEnv "PORT")
  withDBPool
    (\pool -> do
       withResource
         pool
         (runReaderT
            (do runMigration migrateAll
                manualMigration))
       app <- liftIO (toWaiAppPlain App {appPool = pool})
       -- Not important for local dev, but will be important when deploying online.
       -- TODO: Middleware for password-protecting the site
       -- TODO: Middleware for blocking non-load-balancer requests
       liftIO (run port (gzip def {gzipFiles = GzipCompress} app)))

withDBPool ::
     (IsPersistBackend b, BaseBackend b ~ SqlBackend)
  => (Pool b -> LoggingT IO a)
  -> IO a
withDBPool cont = do
  dbstr <- getEnv "DBCONN"
  runStdoutLoggingT
    (filterLogger
       (\_src _lvl -> False)
       (withBackendPool (S8.pack dbstr) 10 cont))
