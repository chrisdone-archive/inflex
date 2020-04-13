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
  , postShopLoginR
  , getShopLoginR
  , getHealthR
  , getHomeR
  ) where

import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Server.App
import           Inflex.Server.View.Shop
import           Lucid
import           Shakespearean
import           Text.Lucius
import           Yesod hiding (Html, Field)
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- Home

getHomeR :: Handler (Html ())
getHomeR =
  htmlWithUrl
    (shopTemplate
       (do h1_ "Inflex"
           p_ "Spreadsheets reimagined."
           url <- ask
           p_
             (a_ [href_ (url ShopRegisterR)]
                 "Register now")))

--------------------------------------------------------------------------------
-- Login

getShopLoginR :: Handler (Html ())
getShopLoginR = pure (pure ())

postShopLoginR :: Handler (Html ())
postShopLoginR = pure (pure ())

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
