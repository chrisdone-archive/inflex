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
  ( postShopAccountR
  , getShopAccountR
  , getHealthR
  , getHomeR
  , getFaviconR
  , getShopCssR
  ) where

import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Lucid
import           Lucid.Base
import           Sendfile
import           Shakespearean
import           Text.Lucius
import           Yesod hiding (Html, Field, lookupSession)
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- Home

getHomeR :: Handler (Html ())
getHomeR = do
  (do msession <- lookupSession
      let state = maybe NoSessionState (sessionState . entityVal) msession
      css <- $(luciusFileFrom "inflex-server/templates/home.lucius")
      htmlWithUrl
        (html_ $ do
           url <- ask
           head_ $ do
             link_ [href_ "#", rel_ "shortcut icon"]
             title_ "Inflex"
             meta_ [content_ "utf-8", name_ "charset"]
             meta_
               [ content_
                   "width=device-width, initial-scale=1, shrink-to-fit=no"
               , name_ "viewport"
               ]
             link_ [href_ (url FaviconR), type_ "image/png", rel_ "icon"]
             script_
               [ async_ ""
               , defer_ ""
               , makeAttribute "data-domain" "inflex.io"
               , src_ "https://plausible.inflex.io/js/index.js"
               ]
               ("" :: Text)
             style_ (LT.toStrict (renderCss css))
           body_ $ do
             div_ [class_ "navbar"] $
               div_ [class_ "margin-wrapper"] $ do
                 div_ [class_ "logo"] (pure ())
                 span_ [class_ "beta-badge"] "beta"
                 div_ [class_ "rhs-nav"] $
                   case state of
                     Registered {} ->
                       form_ [action_ (url AppDashboardR), method_ "get"] $ do
                         button_ [class_ "logout full-button"] "Dashboard"
                     _ ->
                       form_ [action_ (url LoginR), method_ "post"] $ do
                         button_ [class_ "logout full-button"] "Login"
             div_ [class_ "hero"] $
               div_ [class_ "margin-wrapper"] $ do
                 div_ [class_ "tagline"] $ do
                   h1_ "It's time to go off grid"
                   h2_ "Online spreadsheets re-invented"
                   button_
                     [class_ "button tagline-action"]
                     "Request early access!"
                 div_ [class_ "hero-pic"] (pure ())
             div_ [class_ "footer"] $ do
               div_ [class_ "margin-wrapper"] $ do
                 p_ "© 2020 Sky Above Limited"
                 p_ "Inflex® is a registered trademark of Sky Above Limited."))


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

getFaviconR :: Handler TypedContent
getFaviconR = $(sendFileFrom "image/png" "inflex-server/img/favicon.png")

getShopCssR :: Handler Css
getShopCssR = $(luciusFileFrom "inflex-server/templates/shop.lucius")
