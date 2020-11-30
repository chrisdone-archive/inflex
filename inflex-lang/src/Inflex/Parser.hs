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

module Inflex.Parser (parseText, parseType, LexParseError(..)) where

import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.List.Split
import qualified Data.Reparsec as Reparsec
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Semigroup.Foldable
import           Data.Sequence (Seq)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Vector as V
import           Inflex.Instances ()
import           Inflex.Lexer
import           Inflex.Location
import           Inflex.Types
import           Optics

--------------------------------------------------------------------------------
-- Parser types

data LexParseError
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
  | ExpectedVariant
  | ExpectedHole
  | ExpectedGlobal
  | ExpectedDecimal
  | ExpectedText
  | ExpectedDecimalType
  | ExpectedIntegerType
  | ExpectedSignature
  | ExpectedEndOfInput
  | ExpectedLet
  | ExpectedCurly
  | ExpectedCloseCurly
  | ExpectedSquare
  | ExpectedCloseSquare
  | ExpectedPeriod
  | ExpectedIn
  | ExpectedEquals
  | ExpectedContinuation
  | ExpectedComma
  | ExpectedOperator
  | EmptyOperand
  | MissingOpRhs
  | OddNumberOfExpressions
  | DuplicateRecordField FieldName
  | InvalidUnaryExpression
  deriving (Eq, Show)

instance Reparsec.NoMoreInput ParseErrors where
  noMoreInputError = liftError NoMoreInput

instance Reparsec.ExpectedEndOfInput ParseErrors where
  expectedEndOfInputError = liftError ExpectedEndOfInput

type Parser a = Reparsec.ParserT (Seq (Located Token)) ParseErrors Identity a

--------------------------------------------------------------------------------
-- Top-level accessor

-- TODO: Add a configuration that includes limits: depth limits, array
-- size limits, record field limits, etc.

-- | Parse a given block of text.
parseText :: FilePath -> Text -> Either LexParseError (Expression Parsed)
parseText fp bs = do
  tokens <- first LexerError (lexText fp bs)
  first
    ParseError
    (runIdentity (Reparsec.parseOnlyT (expressionParser <* Reparsec.endOfInput) tokens))

-- | Parse a given block of type.
parseType :: FilePath -> Text -> Either LexParseError (Type Parsed)
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
operatorPrecedence = ["=", "-", "+", "*", "/"]

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
resolveChain xs0 = go operatorPrecedence xs0
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
unchainedExpressionParser = unary <> operatorlessExpressionParser
  where
    unary = do
      operator <- operatorParser
      expr <- operatorlessExpressionParser
      case (operator, expr) of
        (Located {thing = "-"}, LiteralExpression (NumberLiteral Number {..})) ->
          pure
            (LiteralExpression
               (NumberLiteral
                  Number
                    { number =
                        case number of
                          IntegerNumber i -> IntegerNumber (negate i)
                          DecimalNumber Decimal {integer, ..} ->
                            DecimalNumber Decimal {integer = negate integer, ..}
                    , ..
                    }))
        _ -> Reparsec.failWith (liftError InvalidUnaryExpression)

operatorlessExpressionParser :: Parser (Expression Parsed)
operatorlessExpressionParser =
  fold1
    (NE.fromList
       [ PropExpression <$> propParser
       , RecordExpression <$> recordParser
       , ArrayExpression <$> arrayParser
       , LetExpression <$> letParser
       , applyParser
       , LiteralExpression <$> literalParser
       , LambdaExpression <$> lambdaParser
       , HoleExpression <$> holeParser
       , VariableExpression <$> variableParser
       , VariantExpression <$> variantParser
       , parensParser
       ])

recordParser :: Parser (Record Parsed)
recordParser = do
  Located {location = SourceLocation {start}} <-
    token ExpectedCurly (preview _OpenCurlyToken)
  fields <-
    let loop seen = do
          nameResult <- fmap Just fieldNameParser <> pure Nothing
          case nameResult of
            Nothing -> pure []
            Just (name, _)
              | Set.member name seen -> Reparsec.failWith (liftError (DuplicateRecordField name))
              | otherwise -> do
                Located {location, thing = ()} <-
                  token ExpectedContinuation (preview _ColonToken)
                expression <- expressionParser
                comma <-
                  fmap (const True) (token_ ExpectedComma (preview _CommaToken)) <>
                  pure False
                rest <-
                  if comma
                    then loop (Set.insert name seen)
                    else pure []
                let field = FieldE {name, expression, location}
                pure (field : rest)
     in loop mempty
  Located {location = SourceLocation {end}} <-
    token ExpectedCloseCurly (preview _CloseCurlyToken)
  pure Record {fields, typ = Nothing, location = SourceLocation {start, end}}

arrayParser :: Parser (Array Parsed)
arrayParser = do
  Located {location = SourceLocation {start}} <-
    token ExpectedSquare (preview _OpenSquareToken)
  quickEnd <-
    fmap
      (\Located {location = SourceLocation {end}} -> pure end)
      (token ExpectedCloseSquare (preview _CloseSquareToken)) <>
    pure Nothing
  (expressions, end) <-
    let loop = do
          expression <- expressionParser
          comma <-
            fmap (const True) (token_ ExpectedComma (preview _CommaToken)) <>
            pure False
          rest <-
            if comma
              then loop
              else pure []
          pure (expression : rest)
     in case quickEnd of
          Nothing -> do
            ret <- loop
            Located {location = SourceLocation {end}} <-
              token ExpectedCloseSquare (preview _CloseSquareToken)
            pure (ret, end)
          Just end -> pure ([], end)
  typ <- optionalSignatureParser
  pure
    Array
      { expressions = V.fromList expressions
      , typ
      , location = SourceLocation {start, end}
      }

propParser :: Parser (Prop Parsed)
propParser = do
  expression <-
    (RecordExpression <$> recordParser) <>
    (HoleExpression <$> holeParser) <>
    (VariableExpression <$> variableParser) <>
    parensParser
  Located {location} <- token ExpectedPeriod (preview _PeriodToken)
  (name, _) <- fieldNameParser
  pure Prop {typ = Nothing, ..}

fieldNameParser :: Parser (FieldName, SourceLocation)
fieldNameParser = do
  Located {thing = name, location} <-
    token ExpectedVariable (preview _LowerWordToken) <>
    token ExpectedVariable (preview _StringToken)
  pure (FieldName name, location)

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
  Located {thing} <- operatorParser
  if thing == "="
    then do
      value <- expressionParser
      let SourceLocation {start} = paramLocation param
          SourceLocation {end} = expressionLocation value
      pure
        Bind
          {param, location = SourceLocation {start, end}, value, typ = Nothing}
    else Reparsec.failWith (liftError ExpectedEquals)

applyParser :: Parser (Expression Parsed)
applyParser = do
  function <- functionParser
  token_ (ExpectedToken OpenRoundToken) (preview _OpenRoundToken)
  let loop = do
        bind <- expressionParser
        comma <-
          fmap
            (const True)
            (token_ (ExpectedToken CommaToken) (preview _CommaToken)) <>
          pure False
        rest <-
          if comma
            then fmap NE.toList loop
            else pure []
        pure (bind :| rest)
  arguments <- loop
  token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)
  typ <- optionalSignatureParser
  pure
    (foldl'
       (\function' (i, argument) ->
          ApplyExpression
            Apply
              { function = function'
              , argument
              , location = expressionLocation argument -- TODO: Look at this.
              , typ = if i == length arguments
                         then typ
                         else Nothing
              })
       function
       (zip [1..] (toList arguments)))

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
       [ HoleExpression <$> holeParser
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
literalParser = number <> text
  where
    number = do
      umber <- numberParser
      pure (NumberLiteral umber)
    text = do
      Located {thing = text', location} <-
        token ExpectedText (preview _StringToken)
      pure (TextLiteral (LiteralText {typ = Nothing, text = text', ..}))

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

variantParser :: Parser (Variant Parsed)
variantParser = do
  token_ ExpectedVariant (preview _HashToken)
  Located {thing = name, location} <-
    token ExpectedVariant (preview _LowerWordToken)
  argument <-
    do parens <-
         fmap
           (const True)
           (token_ (ExpectedToken OpenRoundToken) (preview _OpenRoundToken)) <>
         pure False
       if parens
         then do
           e <- expressionParser
           token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)
           pure (pure e)
         else pure Nothing
  pure Variant {tag = TagName name, location, typ = Nothing, argument}

holeParser :: Parser (Hole Parsed)
holeParser = do
  Located {thing = _name, location} <-
    token ExpectedHole (preview _HoleToken)
  pure Hole {location, typ = Nothing}

lambdaParser :: Parser (Lambda Parsed)
lambdaParser = do
  param <- paramParser
  token_ (ExpectedToken ColonToken) (preview _ColonToken)
  body <- expressionParser
  let SourceLocation {start} = paramLocation param
      SourceLocation {end} = expressionLocation body
  pure
    Lambda
      {location = SourceLocation {start, end}, body, typ = Nothing, param = param}

paramParser :: Parser (Param Parsed)
paramParser = do
  Located {location, thing} <- token ExpectedParam (preview _LowerWordToken)
  pure Param {name = thing, location, typ = Nothing}

typeParser :: Parser (Type Parsed)
typeParser = do
  functionType <> decimalType <> integerType <> parensType <> arrayType <>
    freshType <>
    recordType
  where
    freshType = do
      Located {location} <- token ExpectedHole (preview _HoleToken)
      pure (FreshType location)
    arrayType = do
      token_ ExpectedSquare (preview _OpenSquareToken)
      typ <- typeParser
      token_ ExpectedCloseSquare (preview _CloseSquareToken)
      pure (ArrayType typ)
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
    recordType = do
      Located {location = SourceLocation {start}} <-
        token ExpectedCurly (preview _OpenCurlyToken)
      fields <-
        let loop seen = do
              nameResult <- fmap Just fieldNameParser <> pure Nothing
              case nameResult of
                Nothing -> pure []
                Just (name, nameLocation)
                  | Set.member name seen ->
                    Reparsec.failWith (liftError (DuplicateRecordField name))
                  | otherwise -> do
                    result <-
                      fmap
                        Just
                        (token ExpectedContinuation (preview _ColonToken)) <>
                      pure Nothing
                    (location, typ) <-
                      case result of
                        Just (Located {location, thing = ()}) -> do
                          typ <- typeParser
                          pure (location, typ)
                        Nothing -> pure (nameLocation, FreshType nameLocation)
                    comma <-
                      fmap
                        (const True)
                        (token_ ExpectedComma (preview _CommaToken)) <>
                      pure False
                    rest <-
                      if comma
                        then loop (Set.insert name seen)
                        else pure []
                    let field = Field {name, typ, location}
                    pure (field : rest)
         in loop mempty
      Located {location = SourceLocation {end}} <-
        token ExpectedCloseCurly (preview _CloseCurlyToken)
      pure
        (RecordType
           (RowType
              TypeRow
                { location = SourceLocation {start, end}
                , typeVariable = Nothing
                , fields = fields
                }))
