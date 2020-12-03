-- | Shared data types.

module Inflex.Rpc where

import Data.Generic.Rep (class Generic)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON)
import Inflex.Json (opts)
import Prelude (class Show, bind, discard, pure, show, (<>))
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (stringify) as J
import Data.Argonaut.Parser (jsonParser) as J
import Data.Either (Either(..))
import Halogen as H
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (error)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Inflex.Schema

rpcLoadDocument :: forall m. MonadAff m => DocumentId -> m (Either String OutputDocument)
rpcLoadDocument = rpcCall "LoadDocument"

rpcRefreshDocument :: forall m. MonadAff m => RefreshDocument -> m (Either String OutputDocument)
rpcRefreshDocument = rpcCall "RefreshDocument"

rpcUpdateDocument :: forall m. MonadAff m => UpdateDocument -> m (Either String UpdateResult)
rpcUpdateDocument = rpcCall "UpdateDocument"



--------------------------------------------------------------------------------
-- API call

-- TODO: Fix the double encoding and double decoding here.
rpcCall
  :: forall m input output i o
  .  MonadAff m
  => GenericEncode i
  => GenericDecode o
  => Generic input i
  => Generic output o
  => Show output
  => Show input
  => String
  -> input
  -> m (Either String output)
rpcCall endpoint0 input =
  H.liftAff
    (do case J.jsonParser (genericEncodeJSON opts input) of
          Left e -> do
            error ("Own JSON was invalid! " <> e)
            pure (Left e)
          Right json -> do
            -- log (show input)
            result <-
              H.liftAff
                (AX.post
                   ResponseFormat.json
                   endpoint
                   (Just (RequestBody.json json)))
            case result of
              Left err -> do
                error
                  ("POST " <> endpoint <>
                   " response failed to decode:" <>
                   AX.printError err)
                pure (Left (AX.printError err))
              Right response ->
                case runExcept
                       (genericDecodeJSON opts (J.stringify (response . body))) of
                  Right r -> do
                    pure (Right r)
                  Left e -> do
                    error ("Failed to decode:" <> show e)
                    pure (Left (show e)))
  where
    endpoint = "/api/rpc/" <> endpoint0
