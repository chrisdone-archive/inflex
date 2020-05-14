{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Parser for Inflex language.

module Inflex.Parser where

import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Reparsec as Reparsec
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Inflex.Lexer

--------------------------------------------------------------------------------
-- AST types

data Expression =
  LiteralExpression Literal
  deriving (Show, Eq, Ord)

data Literal =
  IntegerLiteral Integery
  deriving (Show, Eq, Ord)

data Integery = Integery
  { location :: Location
  , integer :: Integer
  } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Parser types

newtype ParseErrors =
  ParseErrors (NonEmpty ParseError)
  deriving (Eq, Show, Semigroup)

liftError :: ParseError -> ParseErrors
liftError = ParseErrors . pure

data ParseError
  = LexerError LexError
  | NoMoreInput
  | ExpectedInteger
  deriving (Eq, Show)

instance Reparsec.NoMoreInput ParseErrors where
  noMoreInputError = liftError NoMoreInput

type Parser a = Reparsec.ParserT (Seq (Located Token)) ParseErrors Identity a

--------------------------------------------------------------------------------
-- Top-level accessor

-- | Parse a given block of text.
parseText :: FilePath -> Text -> Either ParseErrors Expression
parseText fp bs = do
  tokens <- first (liftError . LexerError) (lexText fp bs)
  runIdentity (Reparsec.parseOnlyT expressionParser tokens)

--------------------------------------------------------------------------------
-- Parsers

expressionParser :: Parser Expression
expressionParser =
  (LiteralExpression <$> literalParser) <> (LiteralExpression <$> literalParser)

literalParser :: Parser Literal
literalParser = do
  Located {thing = integer, location} <-
    Reparsec.satisfy
      (\case
         l@Located {thing = IntegerToken i} -> pure (fmap (const i) l)
         _ -> Left (liftError ExpectedInteger))
  pure (IntegerLiteral (Integery {integer, location}))
