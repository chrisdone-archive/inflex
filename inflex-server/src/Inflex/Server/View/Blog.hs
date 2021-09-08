{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- |

module Inflex.Server.View.Blog where

import Control.Monad.Reader
import Data.Text (Text)
import Inflex.Server.App
import Inflex.Server.Session
import Lucid
import Lucid.Base
import Yesod.Lucid

blogTemplate :: HasLoginCookie -> Lucid App () -> Lucid App ()
blogTemplate hasLoginCookie' body = do
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
              link_ [rel_ "icon", type_ "image/png", href_ (url (StaticR img_favicon_png))]
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
                            (case hasLoginCookie' of
                               NoLoginCookie ->
                                 form_
                                   [action_ (url LoginR), method_ "get"]
                                   (button_ [class_ "login full-button"] "Login")
                               HasLoginCookie _loginState -> do
                                 form_
                                   [action_ (url LogoutR), method_ "post"]
                                   (button_
                                      [class_ "logout full-button"]
                                      "Logout")))
                    body)))
