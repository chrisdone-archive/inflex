-- |

module Inflex.Types where

import Data.UUID (UUID)
import Inflex.Frisson (View)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Inflex.Schema (Result, Text, Hash)
import Prelude

data OutputCell = OutputCell
  { uuid :: UUID
  , name :: Text
  , code :: Text
  , codeHash :: Hash
  , result :: View Result
  , resultHash :: Hash
  , order :: Int
  }

derive instance genericOutputCell :: Generic OutputCell _
instance showOutputCell :: Show OutputCell where show = genericShow
