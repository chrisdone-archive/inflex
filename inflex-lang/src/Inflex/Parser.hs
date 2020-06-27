{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Reparsec as Reparsec
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Semigroup.Foldable
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Inflex.Instances ()
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
  | ExpectedGlobal
  | ExpectedDecimal
  | ExpectedDecimalType
  | ExpectedIntegerType
  | ExpectedSignature
  | ExpectedEndOfInput
  deriving (Eq, Show)

instance Reparsec.NoMoreInput ParseErrors where
  noMoreInputError = liftError NoMoreInput

instance Reparsec.ExpectedEndOfInput ParseErrors where
  expectedEndOfInputError = liftError ExpectedEndOfInput

type Parser a = Reparsec.ParserT (Seq (Located Token)) ParseErrors (Reader (Set Text)) a

--------------------------------------------------------------------------------
-- Top-level accessor

-- | Parse a given block of text.
parseText :: FilePath -> Text -> Either RenameParseError (Expression Parsed)
parseText fp bs = do
  tokens <- first LexerError (lexText fp bs)
  first
    ParseError
    (runReader (Reparsec.parseOnlyT (expressionParser <* Reparsec.endOfInput) tokens) mempty)

-- | Parse a given block of type.
parseType :: FilePath -> Text -> Either RenameParseError (Type Parsed)
parseType fp bs = do
  tokens <- first LexerError (lexText fp bs)
  first
    ParseError
    (runReader (Reparsec.parseOnlyT typeParser tokens) mempty)

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
       , GlobalExpression <$> globalParser
       , VariableExpression <$> variableParser
       , parensParser
       ])

applyParser :: Parser (Apply Parsed)
applyParser = do
  function <- functionParser
  argument <- argumentParser
  typ <- optionalSignatureParser
  let SourceLocation {start} = expressionLocation function
      SourceLocation {end} = expressionLocation argument
  pure
    Apply
      {function, argument, location = SourceLocation {start, end}, typ}

optionalSignatureParser :: Parser (Maybe (Type Parsed))
optionalSignatureParser = do
  continue <-
    fmap (const True) (token_ ExpectedSignature (preview _DoubleColonToken)) <>
    pure False
  if continue
    then fmap Just typeParser
    else pure Nothing

functionParser :: Parser (Expression Parsed)
functionParser =
  fold1
    (NE.fromList
       [ VariableExpression <$> variableParser
       , GlobalExpression <$> globalParser
       , parensParser
       ])

argumentParser :: Parser (Expression Parsed)
argumentParser =
  fold1
    (NE.fromList
       [ VariableExpression <$> variableParser
       , GlobalExpression <$> globalParser
       , LiteralExpression <$> literalParser
       , LambdaExpression <$> lambdaParser
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
  pure (Number {typ = Nothing, ..})

-- TODO: Add negation.
integerParser :: Parser (Located SomeNumber)
integerParser =
  fmap (fmap (IntegerNumber . fromIntegral)) (token ExpectedInteger (preview _NaturalToken))

-- TODO: Add negation.
decimalParser :: Parser (Located SomeNumber)
decimalParser =
  fmap (fmap DecimalNumber) (token ExpectedDecimal (preview _DecimalToken))

variableParser :: Parser (Variable Parsed)
variableParser = do
  Located {thing = name, location} <-
    token ExpectedVariable (preview _LowerWordToken)
  scope <- ask
  if Set.member name scope
    then pure Variable {name, location, typ = Nothing}
    else Reparsec.failWith (liftError ExpectedVariable)

globalParser :: Parser (Global Parsed)
globalParser = do
  Located {thing = name, location} <-
    token ExpectedGlobal (preview _LowerWordToken)
  scope <- ask
  if Set.member name scope
     then Reparsec.failWith (liftError ExpectedGlobal)
     else pure Global {name, location, scheme = ParsedScheme}

lambdaParser :: Parser (Lambda Parsed)
lambdaParser = do
  Located {location = SourceLocation {start}} <-
    token (ExpectedToken BackslashToken) (preview _BackslashToken)
  param@Param {name} <- paramParser
  token_ (ExpectedToken RightArrowToken) (preview _RightArrowToken)
  body <- local (Set.insert name) expressionParser
  let SourceLocation {end} = expressionLocation body
  pure
    Lambda
      {location = SourceLocation {start, end}, body, typ = Nothing, param = param}

paramParser :: Parser (Param Parsed)
paramParser = do
  Located {location, thing} <- token ExpectedParam (preview _LowerWordToken)
  pure Param {name = thing, location, typ = Nothing}

typeParser :: Parser (Type Parsed)
typeParser = do
  functionType <> decimalType <> integerType <> parensType
  where
    atomicType = decimalType <> integerType <> parensType
    parensType = do
      token_ (ExpectedToken OpenRoundToken) (preview _OpenRoundToken)
      t <- typeParser
      token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)
      pure t
    functionType = do
      f <- atomicType
      Located {location} <- token ExpectedDecimalType (preview _RightArrowToken)
      x <- typeParser
      pure
        (ApplyType
           TypeApplication
             { function =
                 ApplyType
                   TypeApplication
                     { function =
                         ConstantType
                           TypeConstant {location, name = FunctionTypeName}
                     , argument = f
                     , location
                     , kind = FunKind TypeKind TypeKind
                     }
             , argument = x
             , location
             , kind = TypeKind
             })
    decimalType = do
      Located { location = decimalLocation@SourceLocation {start}
              , thing = typeName
              } <-
        token
          ExpectedDecimalType
          (\case
             UpperWordToken "Decimal" -> pure DecimalTypeName
             _ -> Nothing)
      Located {location = placesLocation@SourceLocation {end}, thing = places} <-
        token ExpectedDecimalType (preview _NaturalToken)
      pure
        (ApplyType
           TypeApplication
             { function =
                 ConstantType
                   TypeConstant {location = decimalLocation, name = typeName}
             , argument =
                 ConstantType
                   TypeConstant
                     {location = placesLocation, name = NatTypeName places}
             , location = SourceLocation {start, end}
             , kind = TypeKind
             })
    integerType = do
      Located {location = integerLocation, thing = typeName} <-
        token
          ExpectedIntegerType
          (\case
             UpperWordToken "Integer" -> pure IntegerTypeName
             _ -> Nothing)
      pure
        (ConstantType TypeConstant {location = integerLocation, name = typeName})
