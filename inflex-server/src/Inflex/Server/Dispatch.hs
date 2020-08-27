{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |

module Inflex.Server.Dispatch () where

import Inflex.Server.App
import Inflex.Server.Handlers.Shop
import Inflex.Server.Handlers.Register
import Inflex.Server.Handlers.Stripe
import Inflex.Server.Handlers.Rpc
import Inflex.Rpc
import Inflex.Server.Handlers.Document
import Inflex.Server.Handlers.Dashboard
import Inflex.Server.Handlers.Auth
import Yesod

$(mkYesodDispatch "App" $(parseRoutesFile "config/routes"))
