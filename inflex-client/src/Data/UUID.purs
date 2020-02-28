-- |

module Data.UUID
  ( genUUIDV4
  , uuidToString
  , UUID
  ) where

import Effect (Effect)
import Prelude

foreign import uuidv4 :: Effect String

newtype UUID = UUID String
derive instance eqUuid :: Eq UUID
derive instance ordUuid :: Ord UUID

genUUIDV4 :: Effect UUID
genUUIDV4 = map UUID uuidv4

uuidToString :: UUID -> String
uuidToString (UUID s) = s
