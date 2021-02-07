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
  , postEarlyAccessRequestR
  , getEarlyAccessRequestsR
  ) where

import           Control.Monad.Reader
import           Data.Csv
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.Tour (tour)
import           Lucid hiding (step_, section_)
import           Lucid.Base
import           Sendfile
import           Shakespearean
import           Text.Julius
import           Text.Lucius
import           Yesod hiding (Html, Field, lookupSession, toHtml)
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- Home

getHomeR :: Handler (Html ())
getHomeR = do
  (do msession <- lookupSession
      let state = maybe NoSessionState (sessionState . entityVal) msession
      case state of
        Registered {} -> redirect AppDashboardR
        _ -> pure ()
      css1 <- $(luciusFileFrom "inflex-server/templates/home.lucius")
      css2 <- $(luciusFileFrom "inflex-server/templates/cell.lucius")
      js' <- $(juliusFileFrom "inflex-server/templates/home.julius")
      logo <- liftIO $(openFileFrom "inflex-server/svg/inflex-logo.svg")
      htmlWithUrl
        (do doctype_
            url <- ask
            html_ [lang_ "en"] $ do
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
                style_ (LT.toStrict (renderCss css1 <> renderCss css2))
              body_ [] $ do
                div_ [class_ "navbar"] $
                  div_ [class_ "margin-wrapper"] $ do
                    div_ [class_ "logo"] (toHtmlRaw logo)
                    span_ [class_ "beta-badge"] "beta"
                    div_ [class_ "rhs-nav"] $ do
                      {-form_
                        [class_ "community-link"]
                        (do a_
                              [ href_ "https://community.inflex.io"
                              , class_ "full-button"
                              ]
                              "Community")-}
                      form_
                        []
                        (do a_ [href_ (url BlogR), class_ "full-button"] "Blog")
                      case state of
                        Registered {} ->
                          form_ [action_ (url AppDashboardR), method_ "get"] $ do
                            a_
                              [ href_ (url AppDashboardR)
                              , class_ "logout full-button"
                              ]
                              "Dashboard"
                        _ ->
                          form_ [action_ (url AppDashboardR), method_ "get"] $ do
                            a_
                              [ href_ (url AppDashboardR)
                              , class_ "logout full-button"
                              ]
                              "Login"
                div_ [class_ "hero"] $
                  div_ [class_ "margin-wrapper"] $ do
                    div_ [class_ "tagline"] $ do
                      h1_ "It's time to go off grid"
                      h2_ "Online spreadsheets re-invented"
                      form_
                        [action_ (url EnterDetailsR), method_ "get"]
                            {-div_
                              [class_ "email-address"]
                              (input_
                                 [ name_ "email"
                                 , type_ "email"
                                 , placeholder_ "Your email address"
                                 , required_ ""
                                 ])-}
                        (do button_ [class_ "button tagline-action"] "Try now!")
                    div_
                      [class_ "hero-pic"]
                      (do button_ [class_ "play", onclick_ "play();"] (pure ()))
                div_ [class_ "tour tour-light"] $ do
                  tour
                  div_ [class_ "margin-wrapper"] $ do
                    h1_ "What are you waiting for?"
                  div_ [class_ "margin-wrapper"] $ do
                    p_ "We're open for beta testing now."
                  div_ [class_ "margin-wrapper"] $ do
                    p_ $
                      form_ [action_ (url EnterDetailsR), method_ "get"] $
                      button_ [class_ "button tagline-action"] "Try now!"
                div_ [class_ "footer"] $ do
                  div_ [class_ "margin-wrapper"] $ do
                    p_ "© 2020 Sky Above Limited"
                    p_ "Inflex® is a registered trademark of Sky Above Limited."
                script_ (LT.toStrict (renderJavascript js'))))

--------------------------------------------------------------------------------
-- Early access request

postEarlyAccessRequestR :: Handler (Html ())
postEarlyAccessRequestR = do
  now <- liftIO getCurrentTime
  memail <- lookupPostParam "email"
  case memail >>= parseEmail of
    Just email ->
      runDB
        (void
           (insertUnique
              EarlyAccessRequest
                { earlyAccessRequestCreated = now
                , earlyAccessRequestEmail = email
                , earlyAccessRequestApproved = Nothing
                }))
    Nothing -> redirect HomeR
  css <- $(luciusFileFrom "inflex-server/templates/home.lucius")
  logo <- liftIO $(openFileFrom "inflex-server/svg/inflex-logo.svg")
  htmlWithUrl
    (html_ $ do
       url <- ask
       head_ $ do
         link_ [href_ "#", rel_ "shortcut icon"]
         title_ "Inflex"
         meta_ [content_ "utf-8", name_ "charset"]
         meta_
           [ content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
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
       body_ [] $ do
         div_ [class_ "navbar"] $
           div_ [class_ "margin-wrapper"] $ do
             div_ [class_ "logo"] (toHtmlRaw logo)
             span_ [class_ "beta-badge"] "beta"
             div_ [class_ "rhs-nav"] (pure ())
         div_ [class_ "hero"] $
           div_ [class_ "margin-wrapper"] $ do
             div_ [class_ "thanks"] $ do
               h1_
                 []
                 (do "Thanks for subscribing!"
                     toHtmlRaw "&#x1f44d;")
               p_
                 []
                 "When the time is right, we'll email you with an invitation link."
               p_
                 []
                 (do "Don't worry, we won't spam you. Any emails will have an 'unsubscribe' button.")
         div_ [class_ "footer"] $ do
           div_ [class_ "margin-wrapper"] $ do
             p_ "© 2020 Sky Above Limited"
             p_ "Inflex® is a registered trademark of Sky Above Limited.")

getEarlyAccessRequestsR :: Handler TypedContent
getEarlyAccessRequestsR = do
  withLogin
    (\_ (LoginState {loginEmail}) ->
       if loginEmail == Email "inflex@chrisdone.com"
         then do
           requests <- runDB (selectList [] [Asc EarlyAccessRequestCreated])
           addHeader "Content-Disposition" $
             T.concat ["attachment; filename=\"", filename, "\""]
           pure
             (TypedContent
                "text/csv"
                (toContent
                   (encode
                      (map
                         (\Entity {entityVal = EarlyAccessRequest {earlyAccessRequestEmail}} ->
                            Only (unEmail earlyAccessRequestEmail))
                         requests))))
         else permissionDenied "not admin")
  where
    filename = "early-access-requests.csv"

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
