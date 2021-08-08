{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Rpc where

import           Data.Aeson
import           Data.Text (Text)
import           Inflex.Schema
import           Inflex.Server.App (Handler, timed, Timed(..))
import           Inflex.Server.Handlers.Rpc
import           Yesod hiding (Html)

postAppRpcR :: Text -> Handler TypedContent
postAppRpcR = selectRep . provideRep . rpcHandler

rpcHandler :: Text -> Handler Encoding
rpcHandler name =
  case name of
    "CsvCheckSchema" -> timed (TimedRpcCall "CsvCheckSchema") $ do
      input <- requireCheckJsonBody
      output <- rpcCsvCheckSchema (input :: CsvImportSpec)
      pure (toEncoding (output :: CsvCheckStatus))

    "CsvGuessSchema" -> timed (TimedRpcCall "CsvGuessSchema") $ do
      input <- requireCheckJsonBody
      output <- rpcCsvGuessSchema (input :: File)
      pure (toEncoding (output :: CsvGuess))

    "CsvImport" -> timed (TimedRpcCall "CsvImport") $ do
      input <- requireCheckJsonBody
      output <- rpcCsvImport (input :: CsvImportFinal)
      pure (toEncoding (output :: OutputDocument))

    "GetFiles" -> timed (TimedRpcCall "GetFiles") $ do
      input <- requireCheckJsonBody
      output <- rpcGetFiles (input :: FileQuery)
      pure (toEncoding (output :: FilesOutput))

    "LoadDocument" -> timed (TimedRpcCall "LoadDocument") $ do
      input <- requireCheckJsonBody
      output <- rpcLoadDocument (input :: DocumentId)
      pure (toEncoding (output :: OutputDocument))

    "RedoDocument" -> timed (TimedRpcCall "RedoDocument") $ do
      input <- requireCheckJsonBody
      output <- rpcRedoDocument (input :: DocumentId)
      pure (toEncoding (output :: OutputDocument))

    "UndoDocument" -> timed (TimedRpcCall "UndoDocument") $ do
      input <- requireCheckJsonBody
      output <- rpcUndoDocument (input :: DocumentId)
      pure (toEncoding (output :: OutputDocument))

    "UpdateDocument" -> timed (TimedRpcCall "UpdateDocument") $ do
      input <- requireCheckJsonBody
      output <- rpcUpdateDocument (input :: UpdateDocument)
      pure (toEncoding (output :: UpdateResult))

    "UpdateSandbox" -> timed (TimedRpcCall "UpdateSandbox") $ do
      input <- requireCheckJsonBody
      output <- rpcUpdateSandbox (input :: UpdateSandbox)
      pure (toEncoding (output :: UpdateResult))


    _ -> Prelude.error "Invalid RPC function."

