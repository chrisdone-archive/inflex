{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Document.Static where

import Inflex.Server.App
import Sendfile
import Shakespearean
import Text.Lucius
import Yesod hiding (Html)

getAppJsR :: Handler TypedContent
getAppJsR = $(sendFileFrom "application/javascript" "inflex-client/app.js")

getVegaJsR :: Handler TypedContent
getVegaJsR = $(sendFileFrom "application/javascript" "inflex-server/js/vega-all.js")

getCodemirrorJsR :: Handler TypedContent
getCodemirrorJsR = $(sendFileFrom "application/javascript" "inflex-server/js/codemirror.js")

getLogoR :: Handler TypedContent
getLogoR = $(sendFileFrom "image/svg+xml" "inflex-server/svg/inflex-logo.svg")

getAppCssR :: Handler Css
getAppCssR = $(luciusFileFrom "inflex-server/templates/app.lucius")

getCellCssR :: Handler Css
getCellCssR = $(luciusFileFrom "inflex-server/templates/cell.lucius")
