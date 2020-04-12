module Main (main) where

import Inflex.Server.App
import Inflex.Server.Dispatch ()
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import System.Environment
import Yesod hiding (Html)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  port <- fmap read (getEnv "PORT")
  app <- toWaiAppPlain App
  run port (gzip def {gzipFiles = GzipCompress} app)
