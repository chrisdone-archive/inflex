-- |

module Inflex.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.UUID (UUID)
import Inflex.Frisson (View)
import Inflex.Schema (Result, Text, Hash, Position)
import Prelude (class Show, class Eq, (==), (&&))

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

-- This instance specifically caters to this line:
-- <https://gitlab-skyabove/sky-above/inflex/blob/ded321713f2920ab3f32b338d97bc1b2ec728e82/inflex-client/src/Inflex/Components/Cell.purs#L214>
--
-- We should instead use a newtype for a specific kind of
-- equality. Because right now this is very specific and if used
-- elsewhere could lead to bugs.
instance outputCellEq :: Eq OutputCell where
  eq (OutputCell{resultHash: h1, name: n1, uuid: u1}) (OutputCell{resultHash: h2, name: n2, uuid: u2}) =
    u1 == u2 &&
    h1 == h2 &&
    n1 == n2

derive instance genericOutputCell :: Generic OutputCell _
instance showOutputCell :: Show OutputCell where show = genericShow
