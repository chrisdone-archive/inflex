{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module Inflex.Server.Handlers.Shop
  ( getShopCssR
  , postShopAccountR
  , getShopAccountR
  , getHealthR
  , getHomeR
  ) where

import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.Shop
import           Lucid
import           Shakespearean
import           Text.Lucius
import           Yesod hiding (Html, Field, lookupSession)
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- Home

getHomeR :: Handler (Html ())
getHomeR = do
  msession <- lookupSession
  let state = maybe NoSessionState (sessionState . entityVal) msession
  htmlWithUrl
    (shopTemplate
       state
       (div_
          [class_ "container-fluid"]
          (div_
             [class_ "row"]
             (div_
                [class_ "col"]
                (do url <- ask
                    case state of
                      NoSessionState {} -> do
                        p_ (a_ [href_ (url EnterDetailsR)] "Register now")
                      Unregistered {} ->
                        p_
                          (a_
                             [href_ (url EnterDetailsR)]
                             "Continue registration")
                      Registered {} ->
                        p_ (a_ [href_ (url AppDashboardR)] "Go to dashboard"))))))

--------------------------------------------------------------------------------
-- Account

getShopAccountR :: Handler (Html ())
getShopAccountR = pure (pure ())

postShopAccountR :: Handler (Html ())
postShopAccountR = pure (pure ())

--------------------------------------------------------------------------------
-- Account

getHealthR :: Handler Text
getHealthR = do
  req <- getRequest
  pure (T.pack (show (reqWaiRequest req)))

--------------------------------------------------------------------------------
-- Statics

getShopCssR :: Handler Css
getShopCssR = $(luciusFileFrom "inflex-server/templates/shop.lucius")
