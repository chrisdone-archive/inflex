{-# LANGUAGE ViewPatterns #-}
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
import           Data.Functor
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Reparsec as Reparsec
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Semigroup.Foldable
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Inflex.Lexer
import           Inflex.Location
import           Inflex.Types
import           Optics

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
  | ExpectedToken Token
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
-- Helpers

token :: ParseError -> (Token -> Maybe a) -> Parser (Located a)
token err parser =
  Reparsec.satisfy
    (\case
       l@Located {thing = (parser -> Just i)} -> pure (fmap (const i) l)
       _ -> Left (liftError err))

token_ :: ParseError -> (Token -> Maybe a) -> Parser ()
token_ err parser =
  void
    (Reparsec.satisfy
       (\case
          l@Located {thing = (parser -> Just i)} -> pure (fmap (const i) l)
          _ -> Left (liftError err)))

--------------------------------------------------------------------------------
-- Parsers

expressionParser :: Parser (Expression Parsed)
expressionParser =
  fold1
    (NE.fromList
       [ LiteralExpression <$> literalParser
       , LambdaExpression <$> lambdaParser
       ])

literalParser :: Parser (Literal Parsed)
literalParser = do
  Located {thing = integer, location} <-
    token ExpectedInteger (preview _IntegerToken)
  pure (IntegerLiteral (Integery {integer, location, typ = ()}))

lambdaParser :: Parser (Lambda Parsed)
lambdaParser = do
  Located {location = Location {start}} <-
    token (ExpectedToken BackslashToken) (preview _BackslashToken)
  token_ (ExpectedToken RightArrowToken) (preview _RightArrowToken)
  body <- expressionParser
  let Location {end} = expressionLocation body
  pure Lambda {location = Location {start, end}, body, typ = ()}
