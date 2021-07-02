-- | Shared data types.

module Inflex.Rpc where

import Timed (timed, timedM)
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (runExcept)
import Data.Argonaut.Parser as J
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (error)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Halogen as H
import Inflex.Frisson (View, unsafeView)
import Inflex.Json (opts)
import Inflex.Schema
import Prelude (class Show, bind, discard, pure, show, (<>), (==))

rpcCsvCheckSchema :: forall m. MonadAff m => CsvImportSpec -> m (Either String (View CsvCheckStatus))
rpcCsvCheckSchema = rpcCall "CsvCheckSchema"

rpcCsvGuessSchema :: forall m. MonadAff m => File -> m (Either String (View CsvGuess))
rpcCsvGuessSchema = rpcCall "CsvGuessSchema"

rpcCsvImport :: forall m. MonadAff m => CsvImportFinal -> m (Either String (View OutputDocument))
rpcCsvImport = rpcCall "CsvImport"

rpcGetFiles :: forall m. MonadAff m => FileQuery -> m (Either String (View FilesOutput))
rpcGetFiles = rpcCall "GetFiles"

rpcLoadDocument :: forall m. MonadAff m => DocumentId -> m (Either String (View OutputDocument))
rpcLoadDocument = rpcCall "LoadDocument"

rpcRedoDocument :: forall m. MonadAff m => DocumentId -> m (Either String (View OutputDocument))
rpcRedoDocument = rpcCall "RedoDocument"

rpcUndoDocument :: forall m. MonadAff m => DocumentId -> m (Either String (View OutputDocument))
rpcUndoDocument = rpcCall "UndoDocument"

rpcUpdateDocument :: forall m. MonadAff m => UpdateDocument -> m (Either String (View UpdateResult))
rpcUpdateDocument = rpcCall "UpdateDocument"

rpcUpdateSandbox :: forall m. MonadAff m => UpdateSandbox -> m (Either String (View UpdateResult))
rpcUpdateSandbox = rpcCall "UpdateSandbox"



--------------------------------------------------------------------------------
-- API call

-- TODO: Fix the double encoding and double decoding here.
rpcCall
  :: forall m input output i
  .  MonadAff m
  => GenericEncode i
  => Generic input i
  => String
  -> input
  -> m (Either String (View output))
rpcCall endpoint0 input =
  H.liftAff
    (do case timed
               "Rpc.genericEncodeJSON"
               (\_ -> (J.jsonParser (genericEncodeJSON opts input))) of
          Left e -> do
            error ("Own JSON was invalid! " <> e)
            pure (Left e)
          Right json -> do
            result <-
              timedM
                "rpcCall.post"
                (H.liftAff
                   (AX.post
                      ResponseFormat.string
                      endpoint
                      (Just (RequestBody.json json))))
            case result of
              Left err -> do
                error
                  ("POST " <> endpoint <>
                   " response failed to decode:" <>
                   AX.printError err)
                pure (Left (AX.printError err))
              Right response -> do
                if response . status == StatusCode 200
                  then do
                    case timed
                           "Rpc.jsonParser"
                           (\_ -> J.jsonParser (response . body)) of
                      Left err -> do
                        error "Failed to parse JSON! This should never happen."
                        pure
                          (Left
                             "Failed to parse JSON! This should never happen.")
                      Right json -> do
                        pure (Right (unsafeView json))
                  else do
                    error "Bad status code"
                    pure (Left "Bad status code"))
  where
    endpoint = "/api/rpc/" <> endpoint0
