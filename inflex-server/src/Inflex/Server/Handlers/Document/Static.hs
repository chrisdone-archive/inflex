{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Document.Static where

import Inflex.Server.App
import Sendfile
import Shakespearean
import Text.Lucius
import Yesod hiding (Html)

getAppCssR :: Handler Css
getAppCssR = $(luciusFileFrom "inflex-server/templates/app.lucius")

getCellCssR :: Handler Css
getCellCssR = $(luciusFileFrom "inflex-server/templates/cell.lucius")
