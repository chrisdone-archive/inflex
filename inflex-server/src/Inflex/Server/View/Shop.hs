{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- |

module Inflex.Server.View.Shop where

import           Control.Monad.Reader
import           Data.FileEmbed
import           Data.FileEmbed.Stack
import           Data.Text (Text)
import           Inflex.Server.App
import           Inflex.Server.Types
import           Lucid
import           Lucid.Base
import           Yesod.Lucid

-- TODO: Drop bootstrap.
shopTemplate :: SessionState -> Lucid App () -> Lucid App ()
shopTemplate state body = do
  doctype_
  url <- ask
  html_
    [lang_ "en"]
    (do head_
          (do link_ [rel_ "shortcut icon", href_ "#"]
              title_ "Inflex"
              script_
                [ async_ ""
                , defer_ ""
                , makeAttribute "data-domain" "inflex.io"
                , src_ "https://plausible.io/js/plausible.js"
                ]
                ("" :: Text)
              meta_ [name_ "charset", content_ "utf-8"]
              meta_
                [ name_ "viewport"
                , content_
                    "width=device-width, initial-scale=1, shrink-to-fit=no"
                ]
              link_ [rel_ "icon", type_ "image/png", href_ (url FaviconR)]
              link_
                [ rel_ "stylesheet noreferer"
                , type_ "text/css"
                , href_
                    "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
                , crossorigin_ "anonymous"
                , integrity_
                    "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"
                ]
              link_ [rel_ "stylesheet", type_ "text/css", href_ (url ShopCssR)])
        body_
          [class_ "shop"]
          (do header_
                [class_ "navbar navbar-light bg-light"]
                (do a_
                      [ class_ "navbar-brand mr-0 mr-md-2 logo-svg"
                      , href_ (url HomeR)
                      ]
                      (toHtmlRaw
                         $(wrapStackRoot "inflex-server/svg/inflex-logo.svg" >>=
                           embedFile))
                    div_
                      [class_ "navbar-nav ml-md-auto"]
                      (case state of
                         NoSessionState -> a_ [href_ (url LoginR)] "Login"
                         Registered _loginState -> do
                           form_
                             [action_ (url LogoutR), method_ "post"]
                             (button_ [class_ "btn-primary btn"] "Logout")
                         Unregistered {} -> a_ [href_ (url LoginR)] "Login"))
              body))
