-- | Fast lexer.

module Inflex.Lexer where

foreign import lexer :: String -> Array {
    tag :: String,
    location :: {
      start :: { line :: Int, column :: Int },
      end :: { line :: Int, column :: Int }
    }
  }
