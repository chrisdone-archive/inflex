-- |

module Inflex.Server.Handlers.Dashboard
  ( postAppDashboardR
  , getAppDashboardR
  ) where

import Inflex.Server.App
import Lucid
import Yesod hiding (Html)

getAppDashboardR :: Handler (Html ())
getAppDashboardR = pure (pure ())

postAppDashboardR :: Handler (Html ())
postAppDashboardR = pure (pure ())
