-- | Fast lexer.

module Inflex.Lexer
  ( lexString
  , Token(..)
  ) where

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.UUID
import Effect (Effect)
import Effect.Exception
import Prelude


--------------------------------------------------------------------------------
-- Types

data Token = UuidToken UUID | PrimToken String | MiscToken
derive instance genericToken :: Generic Token _
instance showToken :: Show Token where show x = genericShow x

--------------------------------------------------------------------------------
-- Functions

lexString :: String -> Effect (Either Error (Array Token))
lexString i =
  try
    (do toks <- lexer i
        pure
          (map
             (\tagged ->
                case tagged . tag of
                  "uuid" -> UuidToken (UUID (tagged . text))
                  "prim" -> PrimToken (tagged . text)
                  _ -> MiscToken)
             toks))

--------------------------------------------------------------------------------
-- Foreign

foreign import lexer :: String -> Effect (Array {
    tag :: String,
    text :: String,
    location :: {
      start :: { line :: Int, column :: Int },
      end :: { line :: Int, column :: Int }
    }
  })
