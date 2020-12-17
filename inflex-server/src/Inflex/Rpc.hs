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

rpcHandler :: Text -> Handler Value
rpcHandler name =
  case name of
    "LoadDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcLoadDocument (input :: DocumentId)
      pure (toJSON (output :: OutputDocument))

    "RedoDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcRedoDocument (input :: DocumentId)
      pure (toJSON (output :: OutputDocument))

    "RefreshDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcRefreshDocument (input :: RefreshDocument)
      pure (toJSON (output :: OutputDocument))

    "UndoDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcUndoDocument (input :: DocumentId)
      pure (toJSON (output :: OutputDocument))

    "UpdateDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcUpdateDocument (input :: UpdateDocument)
      pure (toJSON (output :: UpdateResult))


    _ -> Prelude.error "Invalid RPC function."

