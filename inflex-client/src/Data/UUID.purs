-- |

module Data.UUID
  ( genUUIDV4
  , uuidToString
  , UUID(..)
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, genericDecode, genericEncode)
import Inflex.Json (opts)
import Prelude (class Eq, class Ord, class Show, map)

foreign import uuidv4 :: Effect String

newtype UUID = UUID String
derive instance eqUuid :: Eq UUID
derive instance ordUuid :: Ord UUID

genUUIDV4 :: Effect UUID
genUUIDV4 = map UUID uuidv4

uuidToString :: UUID -> String
uuidToString (UUID s) = s

derive instance genericUUID :: Generic UUID _
instance showUUID :: Show UUID where show = genericShow
instance decodeUUID :: Decode UUID where decode = genericDecode opts
instance encodeUUID :: Encode UUID where encode = genericEncode opts
