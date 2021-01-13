-- |

module Data.Units where

import           Data.ByteUnits
import           Data.Text (Text)
import qualified Data.Text as T

bytesShorthand :: Integral a => a -> Text
bytesShorthand bytes =
  T.pack $
  getShortHand $ getAppropriateUnits $ ByteValue (fromIntegral bytes) Bytes
