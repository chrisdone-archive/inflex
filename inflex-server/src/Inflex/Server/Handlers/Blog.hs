{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Server.Handlers.Blog where

import           Control.Monad.Reader
import           Data.Foldable
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types.Blog
import           Lucid
import           Lucid.Base
import           Sendfile
import           Shakespearean
import           Text.Julius
import           Text.Lucius
import           Yesod hiding (Html, Field, lookupSession)
import           Yesod.Lucid

getBlogR :: Handler (Html ())
getBlogR = do
  hasLoginCookie' <- hasLoginCookie
  css <- $(luciusFileFrom "inflex-server/templates/blog.lucius")
  js' <- $(juliusFileFrom "inflex-server/templates/blog.julius")
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
             ul_
               (traverse_
                  (\(blogEntryName :: BlogEntryName) ->
                     li_
                       (a_
                          [href_ (url (BlogEntryR blogEntryName))]
                          (Lucid.toHtml (show blogEntryName))))
                  [minBound .. maxBound])
         div_ [class_ "footer"] $ do
           div_ [class_ "margin-wrapper"] $ do
             p_ "© 2020 Sky Above Limited"
             p_ "Inflex® is a registered trademark of Sky Above Limited."
         script_ (LT.toStrict (renderJavascript js')))

getBlogEntryR :: BlogEntryName -> Handler (Html ())
getBlogEntryR entryName = do
  hasLoginCookie' <- hasLoginCookie
  css <- $(luciusFileFrom "inflex-server/templates/blog.lucius")
  js' <- $(juliusFileFrom "inflex-server/templates/blog.julius")
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
             ul_
               (traverse_
                  (\(blogEntryName :: BlogEntryName) ->
                     li_
                       (a_
                          [href_ (url (BlogEntryR blogEntryName))]
                          (Lucid.toHtml (show blogEntryName))))
                  [minBound .. maxBound])
         div_ [class_ "footer"] $ do
           div_ [class_ "margin-wrapper"] $ do
             p_ "© 2020 Sky Above Limited"
             p_ "Inflex® is a registered trademark of Sky Above Limited."
         script_ (LT.toStrict (renderJavascript js')))
