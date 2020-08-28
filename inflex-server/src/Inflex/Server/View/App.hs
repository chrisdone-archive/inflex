{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- |

module Inflex.Server.View.App where

import Control.Monad.Reader
import Data.FileEmbed
import Data.FileEmbed.Stack
import Inflex.Server.App
import Inflex.Server.Types
import Lucid
import Yesod.Lucid

appTemplate :: SessionState -> Lucid App () -> Lucid App ()
appTemplate state body = do
  doctype_
  url <- ask
  html_
    [lang_ "en"]
    (do head_
          (do link_ [rel_ "shortcut icon", href_ "#"]
              title_ "Inflex"
              meta_ [name_ "charset", content_ "utf-8"]
              meta_
                [ name_ "viewport"
                , content_
                    "width=device-width, initial-scale=1, shrink-to-fit=no"
                ]
              link_ [rel_ "stylesheet", type_ "text/css", href_ (url AppCssR)])
        body_
          [class_ "app"]
          (do header_
                [class_ "navbar"]
                (do a_
                      [href_ (url HomeR), class_ "nav-logo"]
                      (toHtmlRaw
                         $(wrapStackRoot "inflex-server/svg/inflex-logo.svg" >>=
                           embedFile))
                    div_
                      [class_ "navbar-controls"]
                      (case state of
                         NoSessionState -> a_ [href_ (url LoginR)] "Login"
                         Registered loginState -> do
                           when
                             False
                             (do a_ [] (toHtml (loginUsername loginState))
                                 " ")
                           form_
                             [action_ (url LogoutR), method_ "post"]
                             (button_ [class_ "logout"] "Logout")
                         Unregistered {} -> mempty))
              body))
