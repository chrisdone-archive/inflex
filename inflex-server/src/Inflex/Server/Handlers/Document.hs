{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE Strict #-}
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

-- -- |

module Inflex.Server.Handlers.Document where
--   ( postAppRefreshR
--   , getAppCssR
--   , getAppJsR
--   , getAppEditorR
--   , postRefreshR
--   , getViewDocumentR
--   ) where

import Control.Monad.Reader
import Data.Aeson
import Database.Persist.Sql
import GA
import Inflex.Server.App
import Inflex.Server.Session
import Inflex.Server.Types
import Inflex.Server.View.App
import Lucid
import Sendfile
import Shakespearean
import Text.Lucius
import Yesod hiding (Html)
import Yesod.Lucid

getAppEditorR :: DocumentSlug -> Handler (Html ())
getAppEditorR slug =
  withLogin
    (\_ state@(LoginState {loginAccountId}) -> do
       submitGA
       documentId <-
         do mdoc <-
              runDB
                (selectFirst
                   [ DocumentAccount ==. fromAccountID loginAccountId
                   , DocumentName ==. slug
                   ]
                   [])
            case mdoc of
              Nothing -> notFound
              Just (Entity documentId _) -> pure (documentId)
       htmlWithUrl
         (appTemplate
            (Registered state)
            (do url <- ask
                script_
                  [type_ "text/javascript"]
                  (do toHtmlRaw "window['inflexMetadata'] = "
                      toHtmlRaw
                        (encode
                           (object
                              ["documentId" .= documentId, "logout" .= url LogoutR])))
                script_ [type_ "text/javascript", src_ (url AppJsR)] "")))

getAppJsR :: Handler TypedContent
getAppJsR = $(sendFileFrom "application/javascript" "inflex-client/app.js")

getLogoR :: Handler TypedContent
getLogoR = $(sendFileFrom "image/svg+xml" "inflex-server/svg/inflex-logo.svg")

getAppCssR :: Handler Css
getAppCssR = $(luciusFileFrom "inflex-server/templates/app.lucius")
