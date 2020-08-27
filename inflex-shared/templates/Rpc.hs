{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Rpc where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text (Text)
import           Database.Persist.Sql
import           Inflex.Schema
import qualified Inflex.Schema as Schema
import           Inflex.Server.App (Handler, App)
import           Inflex.Server.Handlers.Rpc
import           Lucid
import           Sendfile
import           Shakespearean
import           Text.Lucius
import           Yesod hiding (Html)
import           Yesod.Lucid

postAppRpcR :: Text -> Handler TypedContent
postAppRpcR name = selectRep (provideRep (rpcHandler name))

$calls
