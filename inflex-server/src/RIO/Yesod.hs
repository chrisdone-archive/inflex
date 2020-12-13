{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | RIO integration for Yesod.

module RIO.Yesod where

import qualified Control.Monad.Logger
import           RIO
import qualified Yesod.Core.Types as Yesod

-- | Log message produced by yesod.
data YesodLog = YesodLog
  { location :: Control.Monad.Logger.Loc
  , source :: LogSource
  , level :: Control.Monad.Logger.LogLevel
  , str :: Control.Monad.Logger.LogStr
  } deriving (Show)

-- | Log a yesod message into the app logger.
yesodLoggerSource ::
     GLogFunc YesodLog
  -> Yesod.Logger
  -> Control.Monad.Logger.Loc
  -> LogSource
  -> Control.Monad.Logger.LogLevel
  -> Control.Monad.Logger.LogStr
  -> IO ()
yesodLoggerSource glogfunc _logger location source level str =
  runReaderT (glog (YesodLog {..})) glogfunc

glogFuncLYesod ::
     Lens app app (GLogFunc msg) (GLogFunc msg)
  -> Lens (Yesod.HandlerData app app) (Yesod.HandlerData app app) (GLogFunc msg) (GLogFunc msg)
glogFuncLYesod logFunc =
  RIO.lens
    (view logFunc . Yesod.rheSite . Yesod.handlerEnv)
    (\handlerData glogFunc ->
       handlerData
         { Yesod.handlerEnv =
             (Yesod.handlerEnv handlerData)
               { Yesod.rheSite =
                   set
                     logFunc
                     (glogFunc)
                     (Yesod.rheSite (Yesod.handlerEnv handlerData))
               }
         })
