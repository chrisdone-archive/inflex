-- |

module Inflex.Json where

import Foreign.Generic (class Decode, class Encode, SumEncoding, defaultOptions, genericDecode, genericEncode)

opts :: { fieldTransform :: String -> String , sumEncoding :: SumEncoding , unwrapSingleArguments :: Boolean , unwrapSingleConstructors :: Boolean}
opts = defaultOptions { unwrapSingleConstructors = true }
