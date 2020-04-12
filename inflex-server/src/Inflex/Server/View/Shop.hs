{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- |

module Inflex.Server.View.Shop where

import Control.Monad.Reader
import Inflex.Server.App
import Lucid
import Yesod.Lucid

shopTemplate :: Lucid App () -> Lucid App ()
shopTemplate body =
  do doctype_
     url <- ask
     html_
       (do head_
             (do link_ [rel_ "shortcut icon", href_ "#"]
                 title_ "Inflex"
                 meta_
                   [ name_ "viewport"
                   , content_ "width=device-width, initial-scale=1.0"
                   ]
                 link_
                   [ rel_ "stylesheet"
                   , type_ "text/css"
                   , href_ (url ShopCssR)
                   ])
           body_
             [class_ "shop"]
             body)
