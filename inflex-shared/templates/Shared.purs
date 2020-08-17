-- | Shared data types.

module Inflex.Shared where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.UUID (UUID)
import Foreign.Generic (class Decode, class Encode, SumEncoding, defaultOptions, genericDecode, genericEncode)
import Inflex.Json (opts)
import Prelude (class Show)

--------------------------------------------------------------------------------
-- Types

type Vector a = Array a

type Text = String

$types

--------------------------------------------------------------------------------
-- Derivings

derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show = genericShow
instance decodeCommand :: Decode Command where decode = genericDecode opts
instance encodeCommand :: Encode Command where encode = genericEncode opts

derive instance genericDocument :: Generic Document _
instance showDocument :: Show Document where show = genericShow
instance decodeDocument :: Decode Document where decode = genericDecode opts
instance encodeDocument :: Encode Document where encode = genericEncode opts

derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where show = genericShow
instance decodeCell :: Decode Cell where decode = genericDecode opts
instance encodeCell :: Encode Cell where encode = genericEncode opts

derive instance genericDocumentId :: Generic DocumentId _
instance showDocumentId :: Show DocumentId where show = genericShow
instance decodeDocumentId :: Decode DocumentId where decode = genericDecode opts
instance encodeDocumentId :: Encode DocumentId where encode = genericEncode opts
