-- | Test file.

module Spec where

import Control.Monad.Except (runExcept)
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
  log (genericEncodeJSON opts (MyRecord { a: 1 }))
  case args of
    [_,_, x] -> log (show (runExcept (genericDecodeJSON opts x :: _ MyRecord)))
    _ -> pure unit
