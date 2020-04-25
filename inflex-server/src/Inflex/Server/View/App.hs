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
              link_
                [ rel_ "stylesheet noreferer"
                , type_ "text/css"
                , href_
                    "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
                , crossorigin_ "anonymous"
                , integrity_
                    "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"
                ]
              link_ [rel_ "stylesheet", type_ "text/css", href_ (url AppCssR)])
        body_
          [class_ "app"]
          (do header_
                [class_ "navbar navbar-light bg-light"]
                (do a_
                      [class_ "navbar-brand mr-0 mr-md-2", href_ (url HomeR)]
                      (toHtmlRaw
                         $(wrapStackRoot "inflex-server/svg/logo.svg" >>=
                           embedFile))
                    div_
                      [class_ "navbar-nav ml-md-auto"]
                      (case state of
                         NoSessionState -> a_ [href_ (url LoginR)] "Login"
                         Registered loginState -> do
                           when
                             False
                             (do a_ [] (toHtml (loginUsername loginState))
                                 " ")
                           form_
                             [action_ (url LogoutR), method_ "post"]
                             (button_ [class_ "btn-primary btn"] "Logout")
                         Unregistered {} -> a_ [href_ (url LoginR)] "Login"))
              body))
