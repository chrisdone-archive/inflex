-- | Shared data types.

module Inflex.Shared where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Generic (class Decode, class Encode, SumEncoding, defaultOptions, genericDecode, genericEncode)
import Prelude (class Show)

--------------------------------------------------------------------------------
-- Types

$types

--------------------------------------------------------------------------------
-- Decoding options

opts :: { fieldTransform :: String -> String , sumEncoding :: SumEncoding , unwrapSingleArguments :: Boolean , unwrapSingleConstructors :: Boolean}
opts = defaultOptions { unwrapSingleConstructors = true }

--------------------------------------------------------------------------------
-- Derivings

derive instance genericMyRecord :: Generic MyRecord _
instance showMyRecord :: Show MyRecord where show = genericShow
instance decodeMyRecord :: Decode MyRecord where decode = genericDecode opts
instance encodeMyRecord :: Encode MyRecord where encode = genericEncode opts
