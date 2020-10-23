{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Rpc where

import           Data.Aeson
import           Data.Text (Text)
import           Inflex.Schema
import           Inflex.Server.App (Handler)
import           Inflex.Server.Handlers.Rpc
import           Yesod hiding (Html)

postAppRpcR :: Text -> Handler TypedContent
postAppRpcR = selectRep . provideRep . rpcHandler

$calls
