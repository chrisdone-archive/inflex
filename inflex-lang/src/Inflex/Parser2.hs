{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}

-- | A very fast parser based on FlatParse.

module Inflex.Parser2
  ( parseText
  , ParseError(..)
  , Env(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Char (isAlphaNum)
import           Data.Coerce
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified FlatParse.Basic as F
import           Inflex.Instances ()
import qualified Inflex.Parser as Parser
import           Inflex.Types

--------------------------------------------------------------------------------
-- Types

data ParseError = Failed deriving (Eq, Show)

newtype Env = Env {original :: ByteString}

--------------------------------------------------------------------------------
-- Top-level parsers

parseText :: FilePath -> Text -> Either ParseError (Expression Parsed)
parseText _fp txt = parseBytes (T.encodeUtf8 txt)

parseBytes :: ByteString -> Either ParseError (Expression Parsed)
parseBytes bs =
  case F.runParser (sourceParser (Env bs)) bs of
    F.OK a remaining ->
      case Parser.parseTextWith
             Parser.optionalSignatureParser
             -- Didn't feel like re-implementing this parser.
             (T.decodeUtf8 remaining) of
        Left {} -> Left Failed
        Right msig ->
          pure
            (case a of
               ArrayExpression array -> ArrayExpression array {typ = msig}
               -- Above: We optionally support type signatures for arrays.
               _ -> a)
    F.Fail -> Left Failed
    F.Err e -> Left e

--------------------------------------------------------------------------------
-- Basic array parser

sourceParser :: Env -> F.Parser ParseError (Expression Parsed)
sourceParser env = whitespace *> expressionParser env

expressionParser :: Env ->  F.Parser ParseError (Expression Parsed)
expressionParser = arrayParser

arrayParser :: Env ->  F.Parser ParseError (Expression Parsed)
arrayParser env = F.branch openBracket elements (recordParser env)
  where
    elements = do
      start <- getSourcePosPrev env
      es <-
        F.many
          (do e <- expressionParser env
              F.optional_ comma
              pure e)
      closeBracket
      end <- getSourcePos env
      pure
        (ArrayExpression
           Array
             { typ = Nothing
             , location = SourceLocation {start = start, end = end}
             , expressions = V.fromList es
             })

recordParser :: Env -> F.Parser ParseError (Expression Parsed)
recordParser env = F.branch openCurly elements (variantParser env)
  where
    elements = do
      start <- getSourcePosPrev env
      fields <-
        F.many
          (do name <- keyParserQuoted F.<|> keyParser
              start' <- getSourcePos env
              colon
              end' <- getSourcePos env
              expression <- expressionParser env
              F.optional_ comma
              pure
                FieldE
                  { name
                  , expression
                  , location = SourceLocation {start = start', end = end'}
                  })
      closeCurly
      end <- getSourcePos env
      pure
        (RecordExpression
           Record
             { typ = Nothing
             , location = SourceLocation {start = start, end = end}
             , fields = fields
             })

stringParser :: Env -> F.Parser ParseError (Expression Parsed)
stringParser env = F.branch speech rest (holeParser env)
  where
    rest = do
      start' <- getSourcePosPrev env
      inner <- F.byteStringOf (F.many_ (F.satisfy (\char -> char /= '"')))
      speech
      end' <- getSourcePos env
      pure
        (LiteralExpression
           (TextLiteral
              (LiteralText
                 { location = SourceLocation {start = start', end = end'}
                 , typ = Nothing
                 , text = T.decodeUtf8 inner
                 , ..
                 })))

holeParser :: Env -> F.Parser ParseError (Expression Parsed)
holeParser env =
  F.branch
    hole
    (do start' <- getSourcePosPrev env
        end' <- getSourcePos env
        pure
          (HoleExpression
             Hole
               { location = SourceLocation {start = start', end = end'}
               , typ = Nothing
               }))
    (numberParser env)

variantParser :: Env -> F.Parser ParseError (Expression Parsed)
variantParser env = F.branch hash rest (stringParser env)
  where
    rest = do
      start' <- getSourcePos env
      name <- keyParserQuoted F.<|> keyParser
      end' <- getSourcePos env
      argument <-
        F.branch
          openRound
          (fmap Just (expressionParser env) <* closeRound)
          (pure Nothing)
      pure
        (VariantExpression
           Variant
             { location = SourceLocation {start = start', end = end'}
             , typ = Nothing
             , tag = TagName (coerce name)
             , argument
             })

numberParser :: Env -> F.Parser ParseError (Expression Parsed)
numberParser env = do
  sign <- F.optional $(F.char '-')
  start <- getSourcePos env
  !number <-
    do i0 <- F.integer
       i <-
         F.optioned
           $(F.char '.')
           (\() -> do
              F.spanned
                F.integer
                (\j (F.Span start' end') -> do
                   let len = fromIntegral (coerce start' - coerce end' :: Int)
                   pure
                     (DecimalNumber
                        (Decimal
                           { places = len
                           , integer =
                               let i = (i0 * (10 ^ len)) + j
                                in if isJust sign
                                     then -i
                                     else i
                           }))))
           (pure
              (IntegerNumber
                 (if isJust sign
                    then -i0
                    else i0)))
       whitespace
       pure i
  end <- getSourcePos env
  pure
    (LiteralExpression
       (NumberLiteral
          Number
            { location = SourceLocation {start = start, end = end}
            , number
            , typ = Nothing
            }))

--------------------------------------------------------------------------------
-- General tokens

-- > Note: it's more efficient to use spanOf and spanned instead.
keyParser :: F.Parser e FieldName
keyParser =
  fmap
    (FieldName . T.decodeUtf8)
    (F.byteStringOf
       (F.some_ (F.satisfy (\char -> isAlphaNum char || char == '_'))))

keyParserQuoted :: F.Parser ParseError FieldName
keyParserQuoted = do
  speech
  inner <- F.byteStringOf (F.some_ (F.satisfy (\char -> char /= '"')))
  speech
  pure (FieldName (T.decodeUtf8 inner))

comma :: F.Parser e ()
comma = $(F.char ',') *> whitespace

hash :: F.Parser e ()
hash = $(F.char '#')

hole :: F.Parser e ()
hole = $(F.char '_')

speech :: F.Parser e ()
speech = $(F.char '"')

colon :: F.Parser e ()
colon = $(F.char ':') *> whitespace

openBracket :: F.Parser e ()
openBracket = $(F.char '[') *> whitespace

closeBracket :: F.Parser e ()
closeBracket = $(F.char ']') *> whitespace

openRound :: F.Parser e ()
openRound = $(F.char '(') *> whitespace

closeRound :: F.Parser e ()
closeRound = $(F.char ')') *> whitespace

openCurly :: F.Parser e ()
openCurly = $(F.char '{') *> whitespace

closeCurly :: F.Parser e ()
closeCurly = $(F.char '}') *> whitespace

whitespace :: F.Parser e ()
whitespace =
  F.many_
    $(F.switch
        [|case _ of
            " " -> pure ()
            "\n" -> pure ()|])

--------------------------------------------------------------------------------
-- Location getting

getSourcePos :: Env -> F.Parser e SourcePos
getSourcePos Env{original} = do
  pos <- F.getPos
  let ~(line, column) =
        case F.posLineCols original [pos] of
          [(line', col)] -> (line'+1, col+1)
          _ -> (0, 0)
   in pure SourcePos {name = "", line, column}

getSourcePosPrev :: Env -> F.Parser e SourcePos
getSourcePosPrev Env{original} = do
  pos <- F.getPos
  let ~(line, column) =
        case F.posLineCols original [pos] of
          [(line', col)] -> (line'+1, col)
          _ -> (0, 0)
   in pure SourcePos {name = "", line, column}
