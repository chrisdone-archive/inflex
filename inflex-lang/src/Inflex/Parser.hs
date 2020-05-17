{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
import           Inflex.Types

--------------------------------------------------------------------------------
-- Parser types

data RenameParseError
  = LexerError LexError
  | ParseError ParseErrors
  deriving (Eq, Show)

newtype ParseErrors =
  ParseErrors (NonEmpty ParseError)
  deriving (Eq, Show, Semigroup)

liftError :: ParseError -> ParseErrors
liftError = ParseErrors . pure

data ParseError
  = NoMoreInput
  | ExpectedInteger
  deriving (Eq, Show)

instance Reparsec.NoMoreInput ParseErrors where
  noMoreInputError = liftError NoMoreInput

type Parser a = Reparsec.ParserT (Seq (Located Token)) ParseErrors Identity a

--------------------------------------------------------------------------------
-- Top-level accessor

-- | Parse a given block of text.
parseText :: FilePath -> Text -> Either RenameParseError (Expression Parsed)
parseText fp bs = do
  tokens <- first LexerError (lexText fp bs)
  first ParseError (runIdentity (Reparsec.parseOnlyT expressionParser tokens))

--------------------------------------------------------------------------------
-- Parsers

expressionParser :: Parser (Expression Parsed)
expressionParser =
  LiteralExpression <$> literalParser

literalParser :: Parser (Literal Parsed)
literalParser = do
  Located {thing = integer, location} <-
    Reparsec.satisfy
      (\case
         l@Located {thing = IntegerToken i} -> pure (fmap (const i) l)
         _ -> Left (liftError ExpectedInteger))
  pure (IntegerLiteral (Integery {integer, location, typ = ()}))
