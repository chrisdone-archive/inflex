-- | Test file.

module Spec where

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Generic (defaultOptions, genericEncodeJSON, genericDecodeJSON, SumEncoding)
import Node.Process (argv)
import Prelude

opts :: { fieldTransform :: String -> String , sumEncoding :: SumEncoding , unwrapSingleArguments :: Boolean , unwrapSingleConstructors :: Boolean}
opts = defaultOptions { unwrapSingleConstructors = true }

newtype MyRecord = MyRecord { a :: Int }
derive instance genericMyRecord :: Generic MyRecord _
instance showMyRecord :: Show MyRecord where show = genericShow

--------------------------------------------------------------------------------
-- Main entry point

main :: Effect Unit
main = do
  args <- argv
  case args of
    [_, _, "parse-and-encode", x] ->
      case runExcept (genericDecodeJSON opts x :: _ MyRecord) of
        Right r -> log (genericEncodeJSON opts r)
        Left _ -> pure unit
    _ -> pure unit
