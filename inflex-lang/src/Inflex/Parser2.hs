{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}

-- | A very fast parser based on FlatParse.

module Inflex.Parser2 where

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

data ParseError = Failed deriving Show

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
recordParser env = F.branch openCurly elements (numberParser env)
  where
    elements = do
      start <- getSourcePos env
      fields <-
        F.many
          (do start' <- getSourcePos env
              name <- keyParser
              colon
              expression <- expressionParser env
              end' <- getSourcePos env
              F.optional_ comma
              pure
                FieldE
                  { name
                  , expression
                  , location = SourceLocation {start = start', end = end'}
                  })
      end <- getSourcePos env
      closeCurly
      pure
        (RecordExpression
           Record
             { typ = Nothing
             , location = SourceLocation {start = start, end = end}
             , fields = fields
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
