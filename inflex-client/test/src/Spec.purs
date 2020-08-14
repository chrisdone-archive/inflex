-- | Test file.

module Spec where

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Generic (defaultOptions, genericEncodeJSON, genericDecodeJSON, SumEncoding, class Decode, class Encode, genericEncode, genericDecode)
import Prelude (class Show, Unit, bind, pure, unit)

opts :: { fieldTransform :: String -> String , sumEncoding :: SumEncoding , unwrapSingleArguments :: Boolean , unwrapSingleConstructors :: Boolean}
opts = defaultOptions { unwrapSingleConstructors = true }

data MyRecord = MyRecord { a :: Int }
derive instance genericMyRecord :: Generic MyRecord _
instance showMyRecord :: Show MyRecord where show = genericShow
instance decodeMyRecord :: Decode MyRecord where decode = genericDecode opts
instance encodeMyRecord :: Encode MyRecord where encode = genericEncode opts

data MyRecord2 = MyRecord2
  { b :: Int
  , myrec :: MyRecord
  , mrec :: Maybe MyRecord
  , arr :: Array MyRecord
  }
derive instance genericMyRecord2 :: Generic MyRecord2 _
instance showMyRecord2 :: Show MyRecord2 where show = genericShow

data MyProductType1 = MyProductType1
  Int
derive instance genericMyProductType1 :: Generic MyProductType1 _
instance showMyProductType1 :: Show MyProductType1 where show = genericShow

data MyProductType2 = MyProductType2
  Int Int
derive instance genericMyProductType2 :: Generic MyProductType2 _
instance showMyProductType2 :: Show MyProductType2 where show = genericShow
instance decodeMyProductType2 :: Decode MyProductType2 where decode = genericDecode opts
instance encodeMyProductType2 :: Encode MyProductType2 where encode = genericEncode opts

data MyProductType3 = MyProductType3
  Int MyProductType2
derive instance genericMyProductType3 :: Generic MyProductType3 _
instance showMyProductType3 :: Show MyProductType3 where show = genericShow

--------------------------------------------------------------------------------
-- Main entry point

foreign import argv :: Effect (Array String)

main :: Effect Unit
main = do
  args <- argv
  -- log (show args)
  case args of
    [_, "MyRecord", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyRecord) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    [_, "MyRecord2", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyRecord2) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    [_, "MyProductType1", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyProductType1) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    [_, "MyProductType2", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyProductType2) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    [_, "MyProductType3", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyProductType3) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    _ -> pure unit
