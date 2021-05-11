{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}

-- | A very fast parser based on FlatParse.

module Inflex.Parser2 where

import           Data.ByteString (ByteString)
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified FlatParse.Basic as F
import           Inflex.Instances ()
import           Inflex.Types

--------------------------------------------------------------------------------
-- Types

data ParseError = Failed deriving Show

--------------------------------------------------------------------------------
-- Top-level parsers

parseText :: FilePath -> Text -> Either ParseError (Expression Parsed2)
parseText _fp txt = parseBytes (T.encodeUtf8 txt)

parseBytes :: ByteString -> Either ParseError (Expression Parsed2)
parseBytes bs =
  case F.runParser sourceParser bs of
    F.OK a _bs -> Right a
    F.Fail -> Left Failed
    F.Err e -> Left e

--------------------------------------------------------------------------------
-- Basic array parser

sourceParser :: F.Parser ParseError (Expression Parsed2)
sourceParser = whitespace *> expressionParser <* F.eof

expressionParser :: F.Parser ParseError (Expression Parsed2)
expressionParser = arrayParser

arrayParser :: F.Parser ParseError (Expression Parsed2)
arrayParser = F.branch openBracket elements recordParser
  where
    elements = do
      loc <- F.getPos
      es <-
        F.many
          (do e <- expressionParser
              F.optional_ comma
              pure e)
      closeBracket
      pure
        (ArrayExpression
           Array {typ = (), location = loc, expressions = V.fromList es})

recordParser :: F.Parser ParseError (Expression Parsed2)
recordParser = F.branch openCurly elements numberParser
  where
    elements = do
      location <- F.getPos
      fields <-
        F.many
          (do name <- keyParser
              colon
              expression <- expressionParser
              F.optional_ comma
              pure FieldE {name, expression, location})
      closeCurly
      pure
        (RecordExpression
           Record {typ = (), location, fields = fields})

numberParser :: F.Parser ParseError (Expression Parsed2)
numberParser = do
  pos <- F.getPos
  i <- integerParser
  pure
    (LiteralExpression
       (NumberLiteral
          Number
            { location = pos
            , number = IntegerNumber i
            , typ = ()
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

integerParser :: F.Parser e Integer
integerParser = F.integer <* whitespace

comma :: F.Parser e ()
comma = $(F.char ',') *> whitespace

colon :: F.Parser e ()
colon = $(F.char ':') *> whitespace

openBracket :: F.Parser e ()
openBracket = $(F.char '[') *> whitespace

closeBracket :: F.Parser e ()
closeBracket = $(F.char ']') *> whitespace

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
