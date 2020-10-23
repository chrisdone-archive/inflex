-- |

module Inflex.Json where

import Foreign.Generic (SumEncoding, defaultOptions)

opts :: { fieldTransform :: String -> String , sumEncoding :: SumEncoding , unwrapSingleArguments :: Boolean , unwrapSingleConstructors :: Boolean}
opts = defaultOptions { unwrapSingleConstructors = true }
