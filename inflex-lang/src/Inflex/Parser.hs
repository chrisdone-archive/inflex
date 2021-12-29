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

module Inflex.Parser
  ( parseText
  , parseTextWith
  , parseTokens
  , parseType
  , numberExpressionParser
  , LexParseError(..)
  , optionalSignatureParser
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Decimal
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
import qualified Data.Text as T
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
  | ExpectedFold
  | ExpectedHole
  | ExpectedBar
  | ExpectedGlobal
  | ExpectedDecimal
  | ExpectedText
  | ExpectedDecimalType
  | ExpectedRec
  | ExpectedIntegerType
  | ExpectedSignature
  | ExpectedEndOfInput
  | ExpectedLet
  | ExpectedCurly
  | ExpectedCloseCurly
  | ExpectedType
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
  | DuplicateCaseAlternative (Param Parsed)
  | InvalidUnaryExpression
  | ExpectedKeyword Text
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
  first ParseError (parseTokens tokens)

-- | Parse from a tokens list.
parseTokens :: (Seq (Located Token)) -> Either ParseErrors (Expression Parsed)
parseTokens tokens = do
  (runIdentity (Reparsec.parseOnlyT (expressionParser <* Reparsec.endOfInput) tokens))

-- | Parse a given block of text.
parseTextWith :: Parser e -> Text -> Either LexParseError e
parseTextWith p bs = do
  tokens <- first LexerError (lexText "thing" bs)
  first
    ParseError
    (runIdentity (Reparsec.parseOnlyT (p <* Reparsec.endOfInput) tokens))

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
operatorPrecedence =
  concat [["=", "/=", "<", ">", "<=", ">="], ["-", "+"], ["*", "/"]]

expressionParser :: Parser (Expression Parsed)
expressionParser = caseParser <> chainParser
  where
    chainParser = do
      chain <- infixChainParser
      resolveChain chain

-- Examples
-- case x { #true: p*2, #false: 12 }
-- case x {#some(p): p*1, #none: 12 }
--
caseParser :: Parser (Expression Parsed)
caseParser = do
  Located {location} <- keyword "if"
  scrutinee <- expressionParser
  token_ ExpectedCurly (preview _OpenCurlyToken)
  alternatives <-
    let loop seen = do
          alternative@Alternative {pattern'} <- alterativeParser
          mparam <-
            case patternParam pattern' of
              Nothing -> pure Nothing
              Just param -> do
                when
                  (Set.member param seen)
                  (Reparsec.failWith
                     (liftError (DuplicateCaseAlternative param)))
                pure (Just param)
          comma <-
            fmap (const True) (token_ ExpectedComma (preview _CommaToken)) <>
            pure False
          rest <-
            if comma
              then fmap toList (loop (maybe seen (flip Set.insert seen) mparam))
              else pure []
          pure (alternative :| rest)
     in loop mempty
  token_ ExpectedCurly (preview _CloseCurlyToken)
  pure (CaseExpression (Case {typ = Nothing, ..}))

alterativeParser :: Parser (Alternative Parsed)
alterativeParser = do
  pattern' <- patternParser
  Located {location} <- token ExpectedContinuation (preview _ColonToken)
  expression <- expressionParser
  pure Alternative {location, ..}

patternParser :: Parser (Pattern Parsed)
patternParser =
  fmap WildPattern holeParser <> fmap ParamPattern paramParser <>
  fmap VariantPattern variantPattern

variantPattern :: Parser (VariantP Parsed)
variantPattern = do
  token_ ExpectedVariant (preview _HashToken)
  Located {thing = name, location} <-
    token ExpectedVariant anyWordTokenPreview <> operatorParser
  argument <-
    do parens <-
         fmap
           (const True)
           (token_ (ExpectedToken OpenRoundToken) (preview _OpenRoundToken)) <>
         pure False
       if parens
         then do
           e <- paramParser
           token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)
           pure (pure e)
         else pure Nothing
  pure VariantP {tag = TagName name, location, argument}

keyword :: Text -> Parser (Located ())
keyword word = do
  located@Located {thing} <- token ExpectedVariable (preview _CamelCaseToken)
  if thing == word
    then pure (void located)
    else Reparsec.failWith (liftError (ExpectedKeyword word))

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
                 , global = Global {location, name = ParsedTextName name, scheme = ParsedScheme}
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
       [ dotFuncParser
       , RecordExpression <$> recordParser
       , ArrayExpression <$> arrayParser
       , applyParser
       , LiteralExpression <$> literalParser
       , LambdaExpression <$> lambdaParser
       , LambdaExpression <$> typedLambdaParser
       , HoleExpression <$> holeParser
       , VariableExpression <$> variableParser
       , GlobalExpression <$> globalParser
       , (VariantExpression <$> variantParser)
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
      , form = ()
      }

propLhsParser :: Parser (Expression Parsed)
propLhsParser =
  (VariantExpression <$> variantParser) <> (RecordExpression <$> recordParser) <>
  (ArrayExpression <$> arrayParser) <>
  (HoleExpression <$> holeParser) <>
  applyParser <>
  (VariableExpression <$> variableParser) <>
  (GlobalExpression <$> globalParser) <>
  parensParser

fieldNameParser :: Parser (FieldName, SourceLocation)
fieldNameParser = do
  Located {thing = name, location} <-
    token ExpectedVariable (anyWordTokenPreview) <>
    token ExpectedVariable (preview _StringToken)
  pure (FieldName name, location)

dotFuncParser :: Parser (Expression Parsed)
dotFuncParser = do
  argument0 <- propLhsParser
  Located {location} <- token ExpectedPeriod (preview _PeriodToken)
  let loop argument = do
        thing <- fmap Left globalParser <> fmap Right fieldNameParser
        expression <-
          case thing of
            Left function -> do
              function' <- finishApplyParser (GlobalExpression function)
              pure
                (ApplyExpression
                   (Apply
                      { function = function'
                      , argument
                      , location
                      , typ = Nothing
                      , style = DotApply
                      }))
            Right (name, _) -> do
              -- TODO: This will create a bad parse. Fix it with a lookahead for (, { and [.
              finishApplyParser
                (PropExpression
                   (Prop {typ = Nothing, expression = argument, ..})) <>
                pure
                  (PropExpression
                     (Prop {typ = Nothing, expression = argument, ..}))
        continue <-
          fmap (const True) (token_ ExpectedPeriod (preview _PeriodToken)) <>
          pure False
        (if continue
           then loop
           else pure)
          expression
  loop argument0

applyParser :: Parser (Expression Parsed)
applyParser = do
  function <- functionParser
  finishApplyParser function

finishApplyParser :: Expression Parsed -> Parser (Expression Parsed)
finishApplyParser function0 =
  tuple function0 -- <> list function0 <> record function0
  where
    _list function = do
      array <- arrayParser
      pure
        (ApplyExpression
           Apply
             { location = expressionLocation (ArrayExpression array)
             , function
             , argument = ArrayExpression array
             , typ = Nothing
             , style = PrefixApply
             })
    _record function = do
      record' <- recordParser
      pure
        (ApplyExpression
           Apply
             { location = expressionLocation (RecordExpression record')
             , function
             , argument = RecordExpression record'
             , typ = Nothing
             , style = PrefixApply
             })
    tuple function = do
      token_ (ExpectedToken OpenRoundToken) (preview _OpenRoundToken)
      emptyOrExpr <-
        fmap
          (const Nothing)
          (token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)) <>
        fmap Just expressionParser
      let loop = do
            comma <-
              fmap
                (const True)
                (token_ (ExpectedToken CommaToken) (preview _CommaToken)) <>
              pure False
            if comma
              then do
                bind <- expressionParser
                rest <- loop
                pure (bind : rest)
              else pure []
      arguments <-
        case emptyOrExpr of
          Just expr -> do
            args <- fmap (expr :) loop
            token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)
            pure args
          Nothing -> pure []
      typ <- optionalSignatureParser
      pure
        (foldl'
           (\function' (i, argument) ->
              ApplyExpression
                Apply
                  { function = function'
                  , argument
                  , location = expressionLocation argument -- TODO: Look at this.
                  , typ =
                      if i == length arguments
                        then typ
                        else Nothing
                  , style = PrefixApply
                  })
           function
           (zip [1 ..] (toList arguments)))

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
       , GlobalExpression <$> globalParser
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
      let loop loc = do
            mresult <-
              fmap Just (token ExpectedText (preview _StringToken)) <>
              pure Nothing
            case mresult of
              Nothing -> pure ([], loc)
              Just Located {thing = text', location} -> do
                (rest, loc') <- loop location
                pure (text' : rest, loc')
      Located {thing = first', location} <-
        token ExpectedText (preview _StringToken)
      (text', location') <- loop location
      let SourceLocation {start} = location
          SourceLocation {end} = location'
      pure
        (TextLiteral
           (LiteralText
              { typ = Nothing
              , text = T.intercalate "\"" (first' : text')
              , location = SourceLocation {start, end}
              , ..
              }))

numberParser :: Parser (Number Parsed)
numberParser = do
  Located {thing = number, location} <- integerParser Leave <> decimalParser Leave
  typ <- optionalSignatureParser
  pure (Number {typ, ..})

data Negate = Negate | Leave

applyNegate :: Num a => Negate -> a -> a
applyNegate Negate = negate
applyNegate Leave = id

-- For the CSV parser. Takes care of negatives here.
numberExpressionParser :: Parser (Expression Parsed)
numberExpressionParser = do
  tweak <-
    fmap (\Located{thing} -> thing) (token
       ExpectedOperator
       (\case
          OperatorToken "-" -> pure Negate
          _ -> Nothing)) <>
    pure Leave
  Located {thing = number, location} <- integerParser tweak <> decimalParser tweak
  pure (LiteralExpression (NumberLiteral (Number {typ = Nothing, ..})))

integerParser :: Negate -> Parser (Located SomeNumber)
integerParser neg =
  fmap
    (fmap (IntegerNumber . fromIntegral . applyNegate neg))
    (token ExpectedInteger (preview _NaturalToken))

decimalParser :: Negate -> Parser (Located SomeNumber)
decimalParser neg =
  fmap
    (fmap
       (\Decimal {..} ->
          DecimalNumber (Decimal {integer = applyNegate neg integer, ..})))
    (token ExpectedDecimal (preview _DecimalToken))

variableParser :: Parser (Variable Parsed)
variableParser = do
  Located {thing = name, location} <-
    token ExpectedVariable (anyWordTokenPreview)
  pure Variable {name, location, typ = Nothing}

globalParser :: Parser (Global Parsed)
globalParser = do
  Located {thing = name, location} <-
    token ExpectedVariable (preview _GlobalToken)
  pure Global {name, location, scheme = ParsedScheme}

variantParser :: Parser (Variant Parsed)
variantParser = do
  token_ ExpectedVariant (preview _HashToken)
  Located {thing = name, location} <-
    token ExpectedVariant (anyWordTokenPreview) <> operatorParser
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
  typ <- optionalSignatureParser
  pure Variant {tag = TagName name, location, typ, argument}

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

typedLambdaParser :: Parser (Lambda Parsed)
typedLambdaParser = do
  token_ (ExpectedToken OpenRoundToken) (preview _OpenRoundToken)
  param <- paramParser
  token_ (ExpectedToken ColonToken) (preview _ColonToken)
  body <- expressionParser
  token_ (ExpectedToken CloseRoundToken) (preview _CloseRoundToken)
  typ <- optionalSignatureParser
  let SourceLocation {start} = paramLocation param
      SourceLocation {end} = expressionLocation body
  pure Lambda {location = SourceLocation {start, end}, body, typ, param = param}

paramParser :: Parser (Param Parsed)
paramParser = do
  Located {location, thing} <- token ExpectedParam (anyWordTokenPreview)
  pure Param {name = thing, location, typ = Nothing}

typeParser :: Parser (Type Parsed)
typeParser =
  functionType <> decimalType <> integerType <> parensType <>
  arrayType <>
  freshType <>
  recordType <>
  variantType
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
             AnyWordToken "Decimal" -> pure DecimalTypeName
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
             AnyWordToken "Integer" -> pure IntegerTypeName
             AnyWordToken "Text" -> pure TextTypeName
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
    variantType = do
      Located {location = SourceLocation {start}} <-
        token
          ExpectedCurly
          (\case
             OperatorToken "<" -> pure ()
             _ -> Nothing)
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
      token_ ExpectedBar (preview _BarToken)
      token_ ExpectedHole (preview _HoleToken) -- Signifies the row type variable. Just () below.
      Located {location = SourceLocation {end}} <-
        token
          ExpectedCloseCurly
          (\case
             OperatorToken ">" -> pure ()
             _ -> Nothing)
      pure
        (VariantType
           (RowType
              TypeRow
                { location = SourceLocation {start, end}
                , typeVariable = Just ()
                , fields = fields
                }))

patternParam :: Pattern s -> Maybe (Param s)
patternParam =
  \case
    ParamPattern param -> pure param
    VariantPattern VariantP {argument} -> argument
    WildPattern{} -> Nothing

anyWordTokenPreview :: Token -> Maybe Text
anyWordTokenPreview = \c -> preview _AnyWordToken c <|> preview _CamelCaseToken c
