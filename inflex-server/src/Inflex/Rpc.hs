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

rpcHandler :: Text -> Handler Value
rpcHandler name =
  case name of
    "LoadDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcLoadDocument (input :: DocumentId)
      pure (toJSON (output :: OutputDocument))

    "RefreshDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcRefreshDocument (input :: RefreshDocument)
      pure (toJSON (output :: OutputDocument))

    "UpdateDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcUpdateDocument (input :: UpdateDocument)
      pure (toJSON (output :: OutputDocument))


    _ -> error "Invalid RPC function."

