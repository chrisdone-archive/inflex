{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Parser for Inflex language.

module Inflex.Parser where

import           Data.Bifunctor
import           Data.Text (Text)
import           Data.Void
import           Inflex.Lexer
import           Optics
import qualified Text.Megaparsec as Mega

--------------------------------------------------------------------------------
-- AST types

data Literal =
  IntegerLiteral Integery
  deriving (Show, Eq, Ord)

data Integery = Integery
  { location :: Location
  , integer :: Integer
  } deriving (Show, Eq, Ord)

data ParseError
  = LexerError LexError
  | ParseError (Mega.ParseError (Located Token) Void)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Top-level accessor

-- | Parse a given block of text.
parseText :: FilePath -> Text -> Either ParseError Literal
parseText fp bs = do
  tokens <- first LexerError (lexText fp bs)
  first ParseError (Mega.runParser (literalParser <* Mega.eof) fp tokens)

--------------------------------------------------------------------------------
-- Parsers

literalParser :: Parser Literal
literalParser = do
  Located {thing = integer, location} <- satisfy (preview _IntegerToken)
  pure (IntegerLiteral (Integery {integer, location}))
