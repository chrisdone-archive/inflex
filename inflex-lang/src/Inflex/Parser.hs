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
import           Data.Semigroup.Foldable
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.List.Split
import qualified Data.Reparsec as Reparsec
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
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
  | ExpectedLet
  | ExpectedCurly
  | ExpectedCloseCurly
  | ExpectedPeriod
  | ExpectedIn
  | ExpectedEquals
  | ExpectedContinuation
  | ExpectedComma
  | ExpectedOperator
  | EmptyOperand
  | MissingOpRhs
  | OddNumberOfExpressions
  deriving (Eq, Show)

instance Reparsec.NoMoreInput ParseErrors where
  noMoreInputError = liftError NoMoreInput

instance Reparsec.ExpectedEndOfInput ParseErrors where
  expectedEndOfInputError = liftError ExpectedEndOfInput

type Parser a = Reparsec.ParserT (Seq (Located Token)) ParseErrors Identity a

--------------------------------------------------------------------------------
-- Top-level accessor

-- | Parse a given block of text.
parseText :: FilePath -> Text -> Either RenameParseError (Expression Parsed)
parseText fp bs = do
  tokens <- first LexerError (lexText fp bs)
  first
    ParseError
    (runIdentity (Reparsec.parseOnlyT (expressionParser <* Reparsec.endOfInput) tokens))

-- | Parse a given block of type.
parseType :: FilePath -> Text -> Either RenameParseError (Type Parsed)
parseType fp bs = do
  tokens <- first LexerError (lexText fp bs)
  first
    ParseError
    (runIdentity (Reparsec.parseOnlyT typeParser tokens))

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

-- Precedence from loosest to tightest.
operatorPrecedence :: [Text]
operatorPrecedence = ["-", "+", "*", "/"]

expressionParser :: Parser (Expression Parsed)
expressionParser = do
  chain <- infixChainParser
  resolveChain chain

infixChainParser :: Parser [Either (Located Text) (Expression Parsed)]
infixChainParser = do
  lhs <- unchainedExpressionParser
  moperator <- fmap Just operatorParser <> pure Nothing
  case moperator of
    Nothing -> pure [Right lhs]
    Just operator -> do
      rhs <- infixChainParser
      pure (Right lhs : Left operator : rhs)

-- | Group by loosest precedence, e.g. for + then fold [x,y,z] into
-- x+y+z. Each x, y and z is also grouped and folded by the next
-- higher precedence operator.
resolveChain ::
     [Either (Located Text) (Expression Parsed)] -> Parser (Expression Parsed)
resolveChain = go operatorPrecedence
  where
    go ::
         [Text]
      -> [Either (Located Text) (Expression Parsed)]
      -> Parser (Expression Parsed)
    go [] es =
      case es of
        [Right e] -> pure e
        [] -> Reparsec.failWith (liftError EmptyOperand)
        [Left _op] -> Reparsec.failWith (liftError MissingOpRhs)
        _ -> Reparsec.failWith (liftError OddNumberOfExpressions)
    go (name:os) es =
      let parts =
            splitWhen ((== Left name) . first (\Located {thing} -> thing)) es
       in case NE.nonEmpty parts of
            Nothing -> Reparsec.failWith (liftError EmptyOperand)
            Just parts1 -> do
              resolvedParts1 <- traverse (go os) parts1
              foldlM1 makeInfixOperator resolvedParts1
      where
        makeInfixOperator ::
             (Expression Parsed)
          -> (Expression Parsed)
          -> Parser (Expression Parsed)
        makeInfixOperator left right = do
          let location =
                SourceLocation
                  { start = start (expressionLocation left)
                  , end = end (expressionLocation right)
                  }
          pure
            (InfixExpression
               Infix
                 { location
                 , global = Global {location, name, scheme = ParsedScheme}
                 , left
                 , right
                 , typ = Nothing
                 })


operatorParser :: Parser (Located Text)
operatorParser = token ExpectedOperator (preview _OperatorToken)

unchainedExpressionParser :: Parser (Expression Parsed)
unchainedExpressionParser =
  fold1
    (NE.fromList
       [ RecordExpression <$> recordParser
       , LetExpression <$> letParser
       , ApplyExpression <$> applyParser
       , LiteralExpression <$> literalParser
       , LambdaExpression <$> lambdaParser
       , PropExpression <$> propParser
       , VariableExpression <$> variableParser
       , parensParser
       ])

recordParser :: Parser (Record Parsed)
recordParser = do
  Located {location = SourceLocation {start}} <-
    token ExpectedCurly (preview _OpenCurlyToken)
  fields <-
    let loop = do
          name <- fieldNameParser
          Located {location, thing = ()} <-
            token ExpectedContinuation (preview _ColonToken)
          expression <- expressionParser
          comma <-
            fmap (const True) (token_ ExpectedComma (preview _CommaToken)) <>
            pure False
          rest <-
            if comma
              then loop
              else pure []
          let field = FieldE {name, expression, location}
          pure (field : rest)
     in loop
  Located {location = SourceLocation {end}} <-
    token ExpectedCloseCurly (preview _CloseCurlyToken)
  pure Record {fields, typ = Nothing, location = SourceLocation {start, end}}

propParser :: Parser (Prop Parsed)
propParser = do
  expression <- VariableExpression <$> variableParser
  Located {location} <- token ExpectedPeriod (preview _PeriodToken)
  name <- fieldNameParser
  pure Prop {typ=Nothing, ..}

fieldNameParser :: Parser FieldName
fieldNameParser = do
  Located {thing = name} <-
    token ExpectedVariable (preview _LowerWordToken)
  pure (FieldName name)

letParser :: Parser (Let Parsed)
letParser = do
  Located {location = SourceLocation {start}} <-
    token ExpectedLet (preview _LetToken)
  let loop = do
        bind <- bindParser
        colon <-
          fmap
            (const True)
            (token_ ExpectedContinuation (preview _SemiColonToken)) <>
          pure False
        rest <-
          if colon
            then fmap NE.toList loop
            else pure []
        pure (bind :| rest)
  binds <- loop
  token_ ExpectedIn (preview _InToken)
  body <- expressionParser
  pure
    Let
      { binds
      , typ = Nothing
      , location = SourceLocation {start, end = end (expressionLocation body)}
      , body = body
      }

bindParser :: Parser (Bind Parsed)
bindParser = do
  param <- paramParser
  token_ ExpectedEquals (preview _EqualsToken)
  value <- expressionParser
  let SourceLocation {start} = paramLocation param
      SourceLocation {end} = expressionLocation value
  pure
    Bind {param, location = SourceLocation {start, end}, value, typ = Nothing}

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
       , parensParser
       ])

argumentParser :: Parser (Expression Parsed)
argumentParser =
  fold1
    (NE.fromList
       [ VariableExpression <$> variableParser
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
  typ <- optionalSignatureParser
  pure (Number {typ, ..})

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
  pure Variable {name, location, typ = Nothing}

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
