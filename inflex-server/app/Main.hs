{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Paths_inflex_client
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
getAppJsR = do
  fp <- liftIO (Paths_inflex_client.getDataFileName "app.js")
  sendFile "application/javascript" fp

main :: IO ()
main = warpEnv App
