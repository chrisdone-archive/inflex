-- |

module Inflex.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.UUID (UUID)
import Inflex.Frisson (View)
import Inflex.Schema (Result, Text, Hash, Position)
import Prelude (class Show, class Eq, (==))

data OutputCell = OutputCell
  { uuid :: UUID
  , name :: Text
  , code :: Text
  , codeHash :: Hash
  , result :: View Result
  , resultHash :: Hash
  , order :: Int
  , position :: View Position
  , dependencies :: Array UUID
  }

instance outputCellEq :: Eq OutputCell where
  eq (OutputCell{resultHash: h1}) (OutputCell{resultHash: h2}) = h1 == h2

derive instance genericOutputCell :: Generic OutputCell _
instance showOutputCell :: Show OutputCell where show = genericShow
