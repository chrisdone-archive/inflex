{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |

module Inflex.Server.Dispatch () where

import Inflex.Server.App
import Inflex.Server.Handlers.Shop
import Inflex.Server.Handlers.Shop.Register
import Inflex.Server.Handlers.Document
import Yesod

$(mkYesodDispatch "App" $(parseRoutesFile "config/routes"))
