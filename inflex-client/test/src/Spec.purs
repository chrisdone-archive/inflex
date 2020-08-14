-- | Test file.

module Spec where

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Generic (defaultOptions, genericEncodeJSON, genericDecodeJSON, SumEncoding, class Decode, class Encode, genericEncode, genericDecode)
import Node.Process (argv)
import Prelude

opts :: { fieldTransform :: String -> String , sumEncoding :: SumEncoding , unwrapSingleArguments :: Boolean , unwrapSingleConstructors :: Boolean}
opts = defaultOptions { unwrapSingleConstructors = true }

data MyRecord = MyRecord { a :: Int }
derive instance genericMyRecord :: Generic MyRecord _
instance showMyRecord :: Show MyRecord where show = genericShow
instance decodeMyRecord :: Decode MyRecord where decode = genericDecode opts
instance encodeMyRecord :: Encode MyRecord where encode = genericEncode opts

data MyRecord2 = MyRecord2 { b :: Int, myrec :: MyRecord, arr :: Array MyRecord }
derive instance genericMyRecord2 :: Generic MyRecord2 _
instance showMyRecord2 :: Show MyRecord2 where show = genericShow

--------------------------------------------------------------------------------
-- Main entry point

main :: Effect Unit
main = do
  args <- argv
  case args of
    [_, _, "MyRecord", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyRecord) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    [_, _, "MyRecord2", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyRecord2) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    _ -> pure unit
