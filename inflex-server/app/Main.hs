{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
  / AppR GET
|]

getAppR :: Handler Html
getAppR = sendFile "text/html" "index.html"

main :: IO ()
main = warpEnv App
