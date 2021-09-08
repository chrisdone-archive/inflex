{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- |

module Inflex.Server.View.App where

import Control.Monad.Reader
import Inflex.Server.App
import Inflex.Server.Types
import Lucid
import Yesod.Lucid

appTemplate :: SessionState -> Lucid App () -> Lucid App ()
appTemplate _state body = do
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
              link_ [rel_ "stylesheet", type_ "text/css", href_ (url AppCssR)]
              link_ [rel_ "stylesheet", type_ "text/css", href_ (url CellCssR)])
        body_ [] body)
