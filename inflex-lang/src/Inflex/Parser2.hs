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

data ParseError = Failed deriving Show

parseText :: FilePath -> Text -> Either ParseError (Expression Parsed2)
parseText _fp txt = parseBytes (T.encodeUtf8 txt)

parseBytes :: ByteString -> Either ParseError (Expression Parsed2)
parseBytes bs =
  case F.runParser src bs of
    F.OK a _bs -> Right a
    F.Fail -> Left Failed
    F.Err e -> Left e

--------------------------------------------------------------------------------
-- Basic array parser

ws :: F.Parser ParseError ()
ws =
  F.many_
    $(F.switch
        [|case _ of
            " " -> pure ()
            "\n" -> pure ()|])

open :: F.Parser ParseError ()
open = do $(F.char '[') >> ws

close :: F.Parser ParseError ()
close = $(F.char ']') >> ws

num :: F.Parser ParseError (Expression Parsed2)
num = do
  pos <- F.getPos
  i <- int
  pure
    (LiteralExpression
       (NumberLiteral
          Number
            { location = pos
            , number = IntegerNumber (fromIntegral i)
            , typ = ()
            }))

sexp :: F.Parser ParseError (Expression Parsed2)
sexp =
  F.branch
    open
    (do loc <- F.getPos
        es <-
          F.many
            (do e <- sexp
                F.optional_ $(F.char ',')
                pure e)
        close
        pure
          (ArrayExpression Array {typ = (), location = loc, expressions = V.fromList es}))
    num

src :: F.Parser ParseError (Expression Parsed2)
src = do
  s <- sexp
  F.eof
  pure s

int :: F.Parser ParseError Int
int = do
  _ <- F.lookahead digit
  i <- snd <$>
   (F.chainr
      (\n (!place, !acc) -> (place * 10, acc + place * n))
      digit
      (pure (1,0)))
  ws
  pure i

digit :: F.Parser ParseError Int
digit = (\c -> ord c - ord '0') <$> F.satisfyASCII F.isDigit
