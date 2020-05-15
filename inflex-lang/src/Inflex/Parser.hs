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
import           Inflex.Stages

--------------------------------------------------------------------------------
-- AST types

data Expression s =
  LiteralExpression (Literal s)

data Literal s =
  IntegerLiteral (Integery s)

data Integery s = Integery
  { location :: StagedLocation s
  , integer :: Integer
  }

deriving instance Show (Expression Parsed)
deriving instance Eq (Expression Parsed)
deriving instance Ord (Expression Parsed)
deriving instance Show (Expression Renamed)
deriving instance Eq (Expression Renamed)
deriving instance Ord (Expression Renamed)
deriving instance Show (Literal Parsed)
deriving instance Eq (Literal Parsed)
deriving instance Ord (Literal Parsed)
deriving instance Show (Literal Renamed)
deriving instance Eq (Literal Renamed)
deriving instance Ord (Literal Renamed)
deriving instance Show (Integery Parsed)
deriving instance Eq (Integery Parsed)
deriving instance Ord (Integery Parsed)
deriving instance Show (Integery Renamed)
deriving instance Eq (Integery Renamed)
deriving instance Ord (Integery Renamed)

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
  pure (IntegerLiteral (Integery {integer, location}))
