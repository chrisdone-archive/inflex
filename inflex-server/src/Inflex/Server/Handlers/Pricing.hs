{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Server.Handlers.Pricing where

import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Inflex.Server.App
import           Inflex.Server.Session
import           Lucid hiding (for_)
import           Lucid.Base
import           Sendfile
import           Shakespearean
import           Text.Julius
import           Text.Lucius
import           Yesod hiding (Html, Field, lookupSession, toHtml)
import           Yesod.Lucid

getPricingR :: Handler (Html ())
getPricingR = do
  hasLoginCookie' <- hasLoginCookie
  css1 <- $(luciusFileFrom "inflex-server/templates/pricing.lucius")
  js' <- $(juliusFileFrom "inflex-server/templates/blog.julius")
  logo <- liftIO $(openFileFromBS "inflex-server/svg/inflex-logo.svg")
  htmlWithUrl
    (html_ $ do
       url <- ask
       head_ $ do
         link_ [href_ "#", rel_ "shortcut icon"]
         title_ "Pricing - Inflex"
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
         style_ (LT.toStrict (renderCss css1))
       body_ [class_ "pricing-page"] $ do
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

             div_
               [class_ "pricing-wrap"]
               (do div_
                     [class_ "subs-explain"]
                     (do p_
                           (do "\n     Inflex is a paid service. If you would like to try it for free, hit our "
                               a_ [href_ "/try"] (do "try")
                               " page.\n    ")
                         p_
                           (do "\n    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam mattis tristique ipsum at fringilla. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. ")
                         p_
                           (do "Praesent ultricies quis est sed facilisis. Suspendisse id leo eget purus euismod elementum non at ligula. Maecenas scelerisque metus erat, eu ultricies erat dictum ac. Ut ut sapien aliquam, malesuada turpis sed, lobortis erat. Nulla euismod enim ipsum, vitae euismod eros porta vel.")
                         p_
                           (do " Pellentesque quis enim in est sodales sagittis. Nulla tincidunt tortor lacus, at feugiat diam interdum at.\n"))
                   div_
                     [class_ "pricing-listing"]
                     (do h1_ (do "$10/month")
                         h2_ [class_ "sub-title"] (do "Standard subscription")
                         p_ [class_ "what-you-get"] (do "What you get:")
                         ul_
                           [class_ "features"]
                           (do li_ (do "Document editor")
                               li_ (do "100 documents")
                               li_ (do "10MB storage"))
                         form_
                           [ action_ (url EnterDetailsR)
                           , method_ "get"
                           ]
                           (do button_ [class_ "button "] (do "Register"))))
         div_ [class_ "footer"] $ do
           div_ [class_ "margin-wrapper"] $ do
             p_ "© 2020 Sky Above Limited"
             p_ "Inflex® is a registered trademark of Sky Above Limited."
         script_ (LT.toStrict (renderJavascript js')))
