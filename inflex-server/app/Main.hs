{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
  /appjs AppJsR GET
  /api/refresh RefreshR POST
  / AppR GET
|]

getAppR :: Handler TypedContent
getAppR = sendFile "text/html" "index.html"

getAppJsR :: Handler TypedContent
getAppJsR = sendFile "application/javascript" "../inflex-client/app.js"

data DecOut = DecOut
  { name :: Text
  , rhs :: Text
  , result :: Text
  } deriving (Generic)
instance ToJSON DecOut where
  toJSON DecOut {name, rhs, result} =
    object ["name" .= name, "rhs" .= rhs, "result" .= result]

data DecIn = DecIn
  { name :: Text
  , rhs :: Text
  } deriving (Generic)
instance FromJSON DecIn where
  parseJSON j = do
    o <- parseJSON j
    DecIn <$> o .: "name" <*> o .: "rhs"

postRefreshR :: Handler TypedContent
postRefreshR =
  selectRep
    (provideRep
       (do json :: HashMap Text DecIn <- requireCheckJsonBody
           -- TODO: Evaluate the decls as a Duet module.
           pure Null))

main :: IO ()
main = warpEnv App
