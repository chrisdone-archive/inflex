{-# LANGUAGE RecordWildCards #-}
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
  | ExpectedParam
  | ExpectedVariable
  | ExpectedDecimal
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
       [ ApplyExpression <$> applyParser
       , LiteralExpression <$> literalParser
       , LambdaExpression <$> lambdaParser
       , VariableExpression <$> variableParser
       , parensParser
       ])

applyParser :: Parser (Apply Parsed)
applyParser = do
  function <- functionParser
  argument <- argumentParser
  let SourceLocation {start} = expressionLocation function
      SourceLocation {end} = expressionLocation function
  pure
    Apply
      {function, argument, location = SourceLocation {start, end}, typ = ()}

functionParser :: Parser (Expression Parsed)
functionParser =
  fold1
    (NE.fromList
       [ VariableExpression <$> variableParser
       , parensParser
       ])

argumentParser :: Parser (Expression Parsed)
argumentParser =
  fold1
    (NE.fromList
       [ VariableExpression <$> variableParser
       , LiteralExpression <$> literalParser
       , LambdaExpression <$> lambdaParser
       , VariableExpression <$> variableParser
       , parensParser
       ])

parensParser :: Parser (Expression Parsed)
parensParser = do
  token_ (ExpectedToken OpenRoundToken) (preview _OpenRoundToken)
  e <- expressionParser
  token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)
  pure e

literalParser :: Parser (Literal Parsed)
literalParser = do
  number <- numberParser
  pure (NumberLiteral number)

numberParser :: Parser (Number Parsed)
numberParser = do
  Located {thing = number, location} <- integerParser <> decimalParser
  pure (Number {typ = (), ..})

integerParser :: Parser (Located SomeNumber)
integerParser =
  fmap (fmap IntegerNumber) (token ExpectedInteger (preview _IntegerToken))

decimalParser :: Parser (Located SomeNumber)
decimalParser =
  fmap (fmap DecimalNumber) (token ExpectedDecimal (preview _DecimalToken))

variableParser :: Parser (Variable Parsed)
variableParser = do
  Located {thing = name, location} <-
    token ExpectedVariable (preview _LowerWordToken)
  pure Variable {name, location, typ = ()}

lambdaParser :: Parser (Lambda Parsed)
lambdaParser = do
  Located {location = SourceLocation {start}} <-
    token (ExpectedToken BackslashToken) (preview _BackslashToken)
  param <- paramParser
  token_ (ExpectedToken RightArrowToken) (preview _RightArrowToken)
  body <- expressionParser
  let SourceLocation {end} = expressionLocation body
  pure
    Lambda
      {location = SourceLocation {start, end}, body, typ = (), param = param}

paramParser :: Parser (Param Parsed)
paramParser = do
  Located {location, thing} <- token ExpectedParam (preview _LowerWordToken)
  pure Param {name = thing, location, typ = ()}
