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

import           Shakespearean
import           Text.Blaze.Renderer.Utf8
import           Text.Julius
import           Text.Lucius
import           Text.Markdown
import           Yesod hiding (Html, Field, lookupSession, toHtml)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Database.Persist.Sql
import           GA
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.App
import           Lucid
import           Lucid.Base
import           RIO (glog)
import           Sendfile
import           Shakespearean
import           Yesod hiding (Html)
import           Yesod.Lucid

data Source = DocumentSource DocumentId | Sandbox deriving (Eq)

data Meta = Meta
  { readonly :: Bool
  , source :: Source
  }

getSandboxR :: Handler (Html ())
getSandboxR = do
  css1 <- $(luciusFileFrom "inflex-server/templates/app.lucius")
  css2 <- $(luciusFileFrom "inflex-server/templates/cell.lucius")
  glog (AnalyticsMsg VisitTry)
  htmlWithUrl
    (appTemplate
       (LT.toStrict (renderCss css1 <> renderCss css2))
       NoSessionState
       (do documentMeta Meta {readonly = False, source = Sandbox}
           documentScripts))

getAppEditorR :: DocumentSlug -> Handler (Html ())
getAppEditorR slug = do
  css1 <- $(luciusFileFrom "inflex-server/templates/app.lucius")
  css2 <- $(luciusFileFrom "inflex-server/templates/cell.lucius")
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
         (appTemplate ((LT.toStrict (renderCss css1 <> renderCss css2)))
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
  , object ["name" .= "array_accum", "display" .= "accum"]
  , object ["name" .= "array_scan", "display" .= "scan"]
  , object ["name" .= "array_reduce", "display" .= "reduce"]
  , object ["name" .= "array_concat", "display" .= "concat"]
  , object ["name" .= "array_find", "display" .= "find"]
  , object ["name" .= "not", "display" .= "not"]
  , object ["name" .= "array_any", "display" .= "any"]
  , object ["name" .= "array_all", "display" .= "all"]
  , object ["name" .= "from_ok", "display" .= "from_ok"]
  ]

documentScripts :: Lucid App ()
documentScripts = do
  url <- ask
  div_
    [class_ "wrapper wrapper-substitute"]
    (do div_
          [class_ "navbar"]
          (do a_ [class_ "logo"] (pure ())
              pure ())
        div_
          [class_ "canvas"]
          (div_
             [class_ "loading-scripts"]
             (div_
                [class_ "lds-ripple"]
                (do div_ (pure ())
                    div_ (pure ())))))
  script_ [type_ "text/javascript", src_ (url (StaticR js_vega_all_js))] ""
  script_ [type_ "text/javascript", src_ (url (StaticR js_codemirror_js))] ""
  script_ [type_ "text/javascript", src_ (url (StaticR js_prosemirror_2021_12_15_js))] ""
  script_
    [type_ "text/javascript", src_ (url (StaticR inflex_client_app_js))]
    ""
