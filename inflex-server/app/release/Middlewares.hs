{-# language OverloadedStrings #-}
-- |

module Middlewares(middlewares) where

import           Control.Concurrent
import           Data.String
import           Data.Time
import           GitInfo (gitHash)
import           Network.Wai
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.Gzip

middlewares :: ThreadId -> UTCTime -> Middleware
middlewares _mainId now = addServerHeader now . gzip def {gzipFiles = GzipCompress}

addServerHeader :: UTCTime -> Middleware
addServerHeader now =
  addHeaders
    [ ("Server", "inflex-server")
    , ("X-Git-Commit", fromString gitHash)
    , ("X-Process-Started", fromString (show now))
    ]
