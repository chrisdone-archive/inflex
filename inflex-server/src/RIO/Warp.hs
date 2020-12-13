{-# LANGUAGE RecordWildCards #-}

-- | Warp logging via RIO's logger.

module RIO.Warp where

import Control.Monad.Reader
import Network.HTTP.Types
import Network.Wai
import RIO

data WaiMsg = WaiMsg
  { request :: Request
  , status :: Status
  , msize :: Maybe Integer
  } deriving (Show)

waiLogger :: GLogFunc WaiMsg -> Request -> Status -> Maybe Integer -> IO ()
waiLogger logFunc request status msize = runReaderT (glog (WaiMsg {..})) logFunc
