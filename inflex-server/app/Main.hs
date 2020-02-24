{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
  /appjs AppJsR GET
  / AppR GET
|]

getAppR :: Handler TypedContent
getAppR = sendFile "text/html" "index.html"

getAppJsR :: Handler TypedContent
getAppJsR = sendFile "application/javascript" "../inflex-client/app.js"

main :: IO ()
main = warpEnv App
