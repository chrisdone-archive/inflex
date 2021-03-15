{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- |

module Inflex.Server.View.Shop where

import           Control.Monad.Reader
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
                , src_ "https://plausible.inflex.io/js/index.js"
                ]
                ("" :: Text)
              meta_ [name_ "charset", content_ "utf-8"]
              meta_
                [ name_ "viewport"
                , content_
                    "width=device-width, initial-scale=1, shrink-to-fit=no"
                ]
              link_ [rel_ "icon", type_ "image/png", href_ (url FaviconR)]
              link_ [rel_ "stylesheet", type_ "text/css", href_ (url ShopCssR)])
        body_
          []
          (do div_
                [class_ "wrapper"]
                (do div_
                      [class_ "navbar"]
                      (do div_ [class_ "logo"] (pure ())
                          div_
                            [class_ "rhs-nav"]
                            (case state of
                               NoSessionState ->
                                 form_
                                   [action_ (url LoginR), method_ "get"]
                                   (button_ [class_ "login full-button"] "Login")
                               Registered _loginState -> do
                                 when
                                   False
                                   (form_
                                      [action_ (url AccountR), method_ "get"]
                                      (button_
                                         [class_ "logout full-button"]
                                         "Account"))
                                 form_
                                   [action_ (url LogoutR), method_ "post"]
                                   (button_
                                      [class_ "logout full-button"]
                                      "Logout")
                               UnregisteredBeta {} ->
                                 form_
                                   [action_ (url LoginR), method_ "get"]
                                   (button_ [class_ "login full-button"] "Login")
                               Unregistered {} ->
                                 form_
                                   [action_ (url LoginR), method_ "get"]
                                   (button_ [class_ "login full-button"] "Login")))
                    body)))
