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

-- -- |

module Inflex.Server.Handlers.Document where
--   ( postAppRefreshR
--   , getAppCssR
--   , getAppJsR
--   , getAppEditorR
--   , postRefreshR
--   , getViewDocumentR
--   ) where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text (Text)
import           Database.Persist.Sql
import           GA
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.App
import           Lucid
import           Lucid.Base
import           RIO (glog)
import           Yesod hiding (Html)
import           Yesod.Lucid

data Source = DocumentSource DocumentId | Sandbox deriving (Eq)

data Meta = Meta
  { readonly :: Bool
  , source :: Source
  }

getSandboxR :: Handler (Html ())
getSandboxR =
  htmlWithUrl
    (appTemplate
       NoSessionState
       (do script_
             [ async_ ""
             , defer_ ""
             , makeAttribute "data-domain" "inflex.io"
             , src_ "https://plausible.inflex.io/js/index.js"
             ]
             ("" :: Text)
           documentMeta Meta {readonly = False, source = Sandbox}
           documentScripts))

getAppEditorR :: DocumentSlug -> Handler (Html ())
getAppEditorR slug =
  withLogin
    (\_ state@(LoginState {loginAccountId}) -> do
       submitGA
       account <- runDB (get404 (fromAccountID loginAccountId))
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
              Just (Entity documentId _) -> do
                glog OpenDocument
                pure (documentId)
       htmlWithUrl
         (appTemplate
            (Registered state)
            (do documentMeta
                  Meta
                    { readonly = not (accountSubscribed account)
                    , source = DocumentSource documentId
                    }
                documentScripts)))

documentMeta :: Meta -> Lucid App ()
documentMeta Meta {..} = do
  url <- ask
  script_
    [type_ "text/javascript"]
    (do toHtmlRaw "window['inflexMetadata'] = "
        toHtmlRaw
          (encode
             (object
                (concat
                   [ case source of
                       DocumentSource documentId -> ["documentId" .= documentId]
                       _ -> []
                   , [ "logout" .= url LogoutR
                     , "dashboard" .= url HomeR
                     , "loggedin" .= True
                     , "readonly" .= readonly
                     , "sandbox" .= (source == Sandbox)
                     , "prims" .= prims
                     ]
                   ]))))

prims :: [Value]
prims =
  [ object ["name" .= "array_map", "display" .= "map"]
  , object ["name" .= "array_filter", "display" .= "filter"]
  , object ["name" .= "array_length", "display" .= "length"]
  , object ["name" .= "array_null", "display" .= "null"]
  , object ["name" .= "vega_raw", "display" .= "vega"]
  , object ["name" .= "array_sum", "display" .= "sum"]
  , object ["name" .= "array_average", "display" .= "average"]
  , object ["name" .= "array_distinct", "display" .= "distinct"]
  , object ["name" .= "array_minimum", "display" .= "minimum"]
  , object ["name" .= "array_maximum", "display" .= "maximum"]
  , object ["name" .= "array_sort", "display" .= "sort"]
  , object ["name" .= "array_concat", "display" .= "concat"]
  , object ["name" .= "array_find", "display" .= "find"]
  , object ["name" .= "array_any", "display" .= "any"]
  , object ["name" .= "array_all", "display" .= "all"]
  , object ["name" .= "from_ok", "display" .= "from_ok"]
  ]

documentScripts :: Lucid App ()
documentScripts = do
  url <- ask
  script_ [type_ "text/javascript", src_ (url VegaJsR)] ""
  script_ [type_ "text/javascript", src_ (url CodemirrorJsR)] ""
  script_ [type_ "text/javascript", src_ (url AppJsR)] ""
