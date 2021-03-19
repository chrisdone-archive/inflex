{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |

module Inflex.Server.Dispatch () where

import Inflex.Rpc
import Inflex.Server.App
import Inflex.Server.Handlers.Auth
import Inflex.Server.Handlers.Blog
import Inflex.Server.Handlers.Dashboard
import Inflex.Server.Handlers.Document
import Inflex.Server.Handlers.Document.Static
import Inflex.Server.Handlers.Register
import Inflex.Server.Handlers.RegisterBeta
import Inflex.Server.Handlers.Shop
import Inflex.Server.Handlers.Stripe
import Inflex.Server.Handlers.Files
import Inflex.Server.Handlers.Legal
import Inflex.Server.Handlers.Docs
import Inflex.Server.Handlers.Portal
import Inflex.Server.Handlers.Account
import Yesod

$(mkYesodDispatch "App" $(parseRoutesFile "config/routes"))
