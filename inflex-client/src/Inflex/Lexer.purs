-- | Fast lexer.

module Inflex.Lexer
  ( lexString
  , Token(..)
  , Location
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

type Location = {
   start :: { line :: Int, column :: Int },
   end :: { line :: Int, column :: Int }
 }

data Token = UuidToken UUID Location | PrimToken String Location | MiscToken
derive instance genericToken :: Generic Token _
instance showToken :: Show Token where show x = genericShow x

--------------------------------------------------------------------------------
-- Functions

lexString :: String -> Effect (Either Error (Array Token))
lexString i =
  try
    (do toks <- lexerWrapped i
        pure
          (map
             (\tagged ->
                case tagged . tag of
                  "uuid" -> UuidToken (UUID (tagged . text)) (tagged.location)
                  "prim" -> PrimToken (tagged . text) (tagged.location)
                  _ -> MiscToken)
             toks))

--------------------------------------------------------------------------------
-- Foreign

foreign import lexerWrapped :: String -> Effect (Array {
    tag :: String,
    text :: String,
    location :: Location
  })
