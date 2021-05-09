{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}

-- | A very fast parser based on FlatParse.

module Inflex.Parser2 where

import           Data.ByteString (ByteString)
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
arrayParser = F.branch openBracket elements numberParser
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

integerParser :: F.Parser ParseError Integer
integerParser = F.integer <* whitespace

comma :: F.Parser ParseError ()
comma = $(F.char ',') *> whitespace

openBracket :: F.Parser ParseError ()
openBracket = $(F.char '[') *> whitespace

closeBracket :: F.Parser ParseError ()
closeBracket = $(F.char ']') *> whitespace

whitespace :: F.Parser ParseError ()
whitespace =
  F.many_
    $(F.switch
        [|case _ of
            " " -> pure ()
            "\n" -> pure ()|])
