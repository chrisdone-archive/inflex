-- |

module Inflex.FieldName where

import Data.Either
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags

validFieldName :: String -> Boolean
validFieldName string =
  case Regex.regex "^[a-zA-Z][A-Za-z0-9_]*$" Flags.noFlags of
    Left _ -> false
    Right regex -> Regex.test regex string
