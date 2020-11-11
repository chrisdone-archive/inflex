{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Server.Handlers.Blog where

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

getBlogR :: Handler (Html ())
getBlogR = do
  hasLoginCookie' <- hasLoginCookie
  css <- $(luciusFileFrom "inflex-server/templates/blog.lucius")
  js' <- $(juliusFileFrom "inflex-server/templates/blog.julius")
  logo <- liftIO $(openFileFrom "inflex-server/svg/inflex-logo.svg")
  articles <-
    liftIO
      (traverse
         (\entryName -> fmap (entryName, ) (getArticleByEntryName entryName))
         (reverse [minBound .. maxBound]))
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
       body_ [class_ "blog-page"] $ do
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
             let byYear (_, Article {date}) = getYear date
                 getYear date =
                   let (year, _, _) = toGregorian date
                    in year
             div_
               [class_ "years"]
               (for_
                  (NE.groupBy (on (==) byYear) (sortOn byYear articles))
                  (\articles' -> do
                     h1_
                       (let (_, Article {date}) = NE.head articles'
                         in toHtml (show (getYear date)))
                     ul_
                       [class_ "year"]
                       (for_
                          articles'
                          (\(blogEntryName, Article {..}) ->
                             li_
                               [class_ "article"]
                               (do h2_
                                     (a_
                                        [href_ (url (BlogEntryR blogEntryName))]
                                        (Lucid.toHtml title))
                                   p_
                                     [class_ "preview"]
                                     (toHtml (showGregorian date)))))))
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
  Article {..} <- liftIO (getArticleByEntryName entryName)
  nextArticle <-
    traverse
      (\nextBlog -> fmap (nextBlog, ) (liftIO (getArticleByEntryName nextBlog)))
      (listToMaybe (drop 1 [entryName ..]))
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
             h1_ (toHtml title)
             p_ (strong_ (toHtml (show date)))
             p_
               (toHtmlRaw
                  (renderMarkup
                     (markdown
                        defaultMarkdownSettings {msAddHeadingId = True}
                        (LT.fromStrict content))))
             for_
               nextArticle
               (\(nextBlogName, Article {title = nextTitle}) -> do
                  hr_ []
                  p_
                    (do strong_ "Next article: "
                        a_
                          [href_ (url (BlogEntryR nextBlogName))]
                          (toHtml nextTitle)))
         div_ [class_ "footer"] $ do
           div_ [class_ "margin-wrapper"] $ do
             p_ "© 2020 Sky Above Limited"
             p_ "Inflex® is a registered trademark of Sky Above Limited."
         script_ (LT.toStrict (renderJavascript js')))
