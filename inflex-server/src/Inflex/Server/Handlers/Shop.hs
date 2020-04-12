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
  , getShopRegisterR
  , postShopRegisterR
  , getHealthR
  , getHomeR
  ) where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Forge.Generate as Forge
import qualified Forge.Internal.Types as Forge
import qualified Forge.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Server.App
import           Inflex.Server.Forge
import           Inflex.Server.Handlers.Forms
import           Inflex.Server.View.Shop
import           Lucid
import           Shakespearean
import           Text.Email.Validate as Email
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
           p_
             (do "A "
                 a_ [href_ "https://skyabove.io/"] "Sky Above"
                 " product.")))

--------------------------------------------------------------------------------
-- Register

verifiedRegisterForm ::
     Monad m
  => Forge.VerifiedForm 'Forge.Unverified Identity (HtmlT m ()) (Field m) Error RegisterSubmit
verifiedRegisterForm = $$($$(Forge.verify [||registerForm||]))

getShopRegisterR :: Handler (Html ())
getShopRegisterR =
  htmlWithUrl
    (shopTemplate
       (do url <- ask
           h1_ "Register"
           form_ [action_ (url ShopRegisterR),method_ "POST"]
                 (do Forge.view verifiedRegisterForm
                     p_ (button_ "Continue"))))

postShopRegisterR :: Handler (Html ())
postShopRegisterR = pure (pure ())

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
