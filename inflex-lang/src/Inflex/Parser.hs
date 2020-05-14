{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Parser for Inflex language.

module Inflex.Parser where

import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.Reparsec as Reparsec
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Data.Void
import           Inflex.Lexer
import           Optics

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
  | NoMoreInput
  | ExpectedInteger
  deriving (Eq, Show)

instance Reparsec.NoMoreInput ParseError where
  noMoreInputError = NoMoreInput

type Parser a = Reparsec.ParserT (Seq (Located Token)) ParseError Identity a

--------------------------------------------------------------------------------
-- Top-level accessor

-- | Parse a given block of text.
parseText :: FilePath -> Text -> Either ParseError Literal
parseText fp bs = do
  tokens <- first LexerError (lexText fp bs)
  runIdentity (Reparsec.parseOnlyT literalParser tokens)

--------------------------------------------------------------------------------
-- Parsers

literalParser :: Parser Literal
literalParser = do
  Located {thing = integer, location} <-
    Reparsec.satisfy
      (\case
         l@Located {thing = IntegerToken i} -> pure (fmap (const i) l)
         _ -> Left ExpectedInteger)
  pure (IntegerLiteral (Integery {integer, location}))
