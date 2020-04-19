{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Dashboard
  ( postAppDashboardR
  , getAppDashboardR
  ) where

import Lucid
import Inflex.Server.App
import Inflex.Server.Session
import Yesod.Lucid

getAppDashboardR :: Handler (Html ())
getAppDashboardR = withLogin (\_ _ -> htmlWithUrl (p_ "Dashboard here!"))

postAppDashboardR :: Handler (Html ())
postAppDashboardR = pure (pure ())
