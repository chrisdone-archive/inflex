{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Server.Handlers.Docs where

import           Control.Monad.Reader
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types.Article
import           Inflex.Server.Types.Blog
import           Lucid hiding (for_)
import           Lucid.Base
import           Sendfile
import           Shakespearean
import           Text.Blaze.Renderer.Utf8
import           Text.Julius
import           Text.Lucius
import           Text.Markdown
import           Yesod hiding (Html, Field, lookupSession, toHtml)
import           Yesod.Lucid

getIntroR :: Handler (Html ())
getIntroR = do
  hasLoginCookie' <- hasLoginCookie
  css1 <- $(luciusFileFrom "inflex-server/templates/blog.lucius")
  css2 <- $(luciusFileFrom "inflex-server/templates/cell.lucius")
  js' <- $(juliusFileFrom "inflex-server/templates/blog.julius")
  logo <- liftIO $(openFileFromBS "inflex-server/static/svg/inflex-logo.svg")
  terms <- liftIO $(openFileFromT "docs/intro.md")
  htmlWithUrl
    (html_ $ do
       url <- ask
       head_ $ do
         link_ [href_ "#", rel_ "shortcut icon"]
         title_ "Intro to Inflex"
         meta_ [content_ "utf-8", name_ "charset"]
         meta_
           [ content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
           , name_ "viewport"
           ]
         link_ [href_ (url (StaticR img_favicon_png)), type_ "image/png", rel_ "icon"]
         style_ (LT.toStrict (renderCss css1 <> renderCss css2))
       body_ [class_ "article-page"] $ do
         div_ [class_ "navbar"] $
           div_ [class_ "margin-wrapper"] $ do
             div_ [class_ "logo"] (a_ [href_ (url HomeR)] (toHtmlRaw logo))
             span_ [class_ "beta-badge"] "beta"
             div_ [class_ "rhs-nav"] $
               case hasLoginCookie' of
                 NoLoginCookie ->
                   form_
                     [action_ (url LoginR), method_ "get"]
                     (button_ [class_ "login full-button"] "Login")
                 HasLoginCookie _loginState -> do
                   form_
                     [action_ (url LogoutR), method_ "post"]
                     (button_ [class_ "logout full-button"] "Logout")
         article_ [class_ "article"] $
           div_ [class_ "margin-wrapper"] $ do
             p_
               (toHtmlRaw
                  (renderMarkup
                     (markdown
                        defaultMarkdownSettings {msAddHeadingId = True}
                        (LT.fromStrict terms))))
         div_ [class_ "footer"] $ do
           div_ [class_ "margin-wrapper"] $ do
             p_ "© 2020 Sky Above Limited"
             p_ "Inflex® is a registered trademark of Sky Above Limited."
         script_ (LT.toStrict (renderJavascript js')))
