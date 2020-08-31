-- | Shared data types.

module Inflex.Schema where

import Data.Generic.Rep.Show (genericShow)
import Data.UUID (UUID)
import Inflex.Json (opts)
import Prelude (class Show, bind, discard, pure, show, ($), (<>))
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (stringify) as J
import Data.Argonaut.Parser (jsonParser) as J
import Data.Either (Either(..))
import Data.Generic.Rep
import Halogen as H
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log, error)
import Foreign.Generic (class Decode, class Encode, genericDecode, genericDecodeJSON, genericEncode, genericEncodeJSON)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)

--------------------------------------------------------------------------------
-- Types

type Vector a = Array a

type Text = String

$types

--------------------------------------------------------------------------------
-- Derivings

derive instance genericNone :: Generic None _
instance showNone :: Show None where show = genericShow
instance decodeNone :: Decode None where decode = genericDecode opts
instance encodeNone :: Encode None where encode = genericEncode opts

derive instance genericResult :: Generic Result _
instance showResult :: Show Result where show = genericShow
instance decodeResult :: Decode Result where decode = genericDecode opts
instance encodeResult :: Encode Result where encode = genericEncode opts

derive instance genericCellError :: Generic CellError _
instance showCellError :: Show CellError where show = genericShow
instance decodeCellError :: Decode CellError where decode = genericDecode opts
instance encodeCellError :: Encode CellError where encode = genericEncode opts

derive instance genericFillError :: Generic FillError _
instance showFillError :: Show FillError where show = genericShow
instance decodeFillError :: Decode FillError where decode = genericDecode opts
instance encodeFillError :: Encode FillError where encode = genericEncode opts

derive instance genericInputDocument :: Generic InputDocument _
instance showInputDocument :: Show InputDocument where show = genericShow
instance decodeInputDocument :: Decode InputDocument where decode = genericDecode opts
instance encodeInputDocument :: Encode InputDocument where encode = genericEncode opts

derive instance genericRefreshDocument :: Generic RefreshDocument _
instance showRefreshDocument :: Show RefreshDocument where show = genericShow
instance decodeRefreshDocument :: Decode RefreshDocument where decode = genericDecode opts
instance encodeRefreshDocument :: Encode RefreshDocument where encode = genericEncode opts

derive instance genericOutputDocument :: Generic OutputDocument _
instance showOutputDocument :: Show OutputDocument where show = genericShow
instance decodeOutputDocument :: Decode OutputDocument where decode = genericDecode opts
instance encodeOutputDocument :: Encode OutputDocument where encode = genericEncode opts

derive instance genericInputCell :: Generic InputCell _
instance showInputCell :: Show InputCell where show = genericShow
instance decodeInputCell :: Decode InputCell where decode = genericDecode opts
instance encodeInputCell :: Encode InputCell where encode = genericEncode opts

derive instance genericOutputCell :: Generic OutputCell _
instance showOutputCell :: Show OutputCell where show = genericShow
instance decodeOutputCell :: Decode OutputCell where decode = genericDecode opts
instance encodeOutputCell :: Encode OutputCell where encode = genericEncode opts

derive instance genericDocumentId :: Generic DocumentId _
instance showDocumentId :: Show DocumentId where show = genericShow
instance decodeDocumentId :: Decode DocumentId where decode = genericDecode opts
instance encodeDocumentId :: Encode DocumentId where encode = genericEncode opts

--------------------------------------------------------------------------------
-- RPC call

-- TODO: Fix the double encoding and double decoding here.
rpcCall
  :: forall m input output i o
  .  MonadAff m
  => GenericEncode i
  => GenericDecode o
  => Generic input i
  => Generic output o
  => Show output
  => String
  -> input
  -> m (Either String output)
rpcCall endpoint0 input =
  H.liftAff
    (do log ("POST " <> endpoint)
        case J.jsonParser (genericEncodeJSON opts input) of
          Left e -> do
            error ("Own JSON was invalid! " <> e)
            pure (Left e)
          Right json -> do
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
              Right response -> do
                log $
                  "POST " <> endpoint <> " response:" <>
                  (J.stringify (response . body))
                case runExcept
                       (genericDecodeJSON opts (J.stringify (response . body))) of
                  Right r -> do
                    log ("OK, decoded:" <> show r)
                    pure (Right r)
                  Left e -> do
                    error ("Failed to decode:" <> show e)
                    pure (Left (show e)))
  where endpoint = "/api/rpc/" <> endpoint0