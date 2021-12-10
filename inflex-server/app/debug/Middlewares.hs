{-# language OverloadedStrings #-}

-- |

module Middlewares (middlewares) where
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

middlewares :: ThreadId -> UTCTime -> Middleware
middlewares mainId time = addKillswitch mainId

addKillswitch :: ThreadId -> Middleware
addKillswitch mainId appl request reply = do
  when (rawPathInfo request == "/kill")
       (killThread mainId)
  appl request reply
