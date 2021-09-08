{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- |

module Inflex.Server.View.App where

import           Control.Monad.Reader
import           Data.Text (Text)
import           Inflex.Server.App
import           Inflex.Server.Types
import           Lucid
import           Yesod.Lucid

appTemplate :: Text ->SessionState -> Lucid App () -> Lucid App ()
appTemplate css _state body = do
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
              style_ css)
        body_ [] (body))
