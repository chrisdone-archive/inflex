{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for Inflex language.

module Inflex.Lexer
  ( Located(..)
  , Token(..)
  , SourcePos(..)
  , SourceLocation(..)
  , lexText
  , LexError
  , lexTextSingleton
  , _NaturalToken
  , _DecimalToken
  , _BackslashToken
  , _RightArrowToken
  , _CamelCaseToken
  , _QuestionToken
  , _AnyWordToken
  , _OpenRoundToken
  , _CloseRoundToken
  , _OpenSquareToken
  , _CloseSquareToken
  , _OpenCurlyToken
  , _CloseCurlyToken
  , _LetToken
  , _InToken
  , _DoubleColonToken
  , _ColonToken
  , _SemiColonToken
  , _OperatorToken
  , _PeriodToken
  , _CommaToken
  , _StringToken
  , _HoleToken
  , _HashToken
  , _GlobalToken
  , _BarToken
  , lexTextPlusUUIDs
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.UUID as UUID
import           Data.Void
import           GHC.Generics
import           Inflex.Instances ()
import           Inflex.Types
import           Inflex.Types.SHA512
import           Numeric.Natural
import           Optics
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Text.Megaparsec.Error

--------------------------------------------------------------------------------
-- Types

-- | Lex text into a series of Tokens.
type Lexer = Mega.Parsec Void Text

-- | Lexical tokens for the Inflex language.
data Token
  = CamelCaseToken !Text
  | AnyWordToken !Text
  | OpenSquareToken
  | CloseSquareToken
  | OpenRoundToken
  | CloseRoundToken
  | NaturalToken !Natural
  | DecimalToken !Decimal
  | BackslashToken
  | RightArrowToken
  | DoubleColonToken
  | ColonToken
  | SemiColonToken
  | CommaToken
  | PeriodToken
  | LetToken
  | InToken
  | OpenCurlyToken
  | CloseCurlyToken
  | OperatorToken !Text
  | StringToken !Text
  | HoleToken
  | HashToken
  | QuestionToken
  | GlobalToken !ParsedGlobal
  | BarToken
  deriving (Show, Eq, Ord, Generic)

-- | A located token.
data Located l = Located
  { location :: SourceLocation
  , thing :: !l
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data LexError =
  LexError (ParseErrorBundle Text Void)
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Entry points

-- | Lex a given block of text.
lexText :: FilePath -> Text -> Either LexError (Seq (Located Token))
lexText fp bs =
  first LexError (Mega.runParser (Mega.space *> tokensLexer <* Mega.eof) fp bs)

-- | Lex a given block of text.
lexTextSingleton :: Text -> Either LexError (Located Token)
lexTextSingleton bs =
  first
    LexError
    (Mega.runParser (Mega.space *> tokenLexer <* Mega.eof) "single-token" bs)

-- | Find UUIDs in a source text, so that we can find any
-- dependencies.
lexTextPlusUUIDs :: Text -> Either LexError (Seq (Located Token), Set Uuid)
lexTextPlusUUIDs text =
  case lexText "" text of
    Left err -> Left err
    Right toks ->
      pure
        ( toks
        , Set.fromList
            (mapMaybe
               (\case
                  Located {thing = GlobalToken (ParsedUuid uuid)} -> pure uuid
                  _ -> Nothing)
               (toList toks)))

--------------------------------------------------------------------------------
-- Lexer

-- | Lex unquoted regular code e.g. @let x = 1@.
tokensLexer :: Lexer (Seq (Located Token))
tokensLexer = fmap Seq.fromList (Mega.many tokenLexer)

-- | Lex a single token.
tokenLexer :: Lexer (Located Token)
tokenLexer =
  Mega.choice [ref, string, camelWord, anyWord, symbol, integer, decimal] <*
  Mega.space
  where
    ref =
      located
        (do void (Mega.char '@')
            uuidRef Mega.<|> primRef Mega.<|> sha512Ref)
      where
        primRef = do
          void (Mega.string "prim:")
          txt <-
            Mega.takeWhile1P Nothing ((||) <$> isAlphaNum <*> flip elem ['_'])
          case M.lookup txt prims of
            Nothing ->
              case txt of
                "from_integer" -> pure (GlobalToken ParsedFromInteger)
                "from_decimal" -> pure (GlobalToken ParsedFromDecimal)
                _ -> fail ("Invalid primitive: " <> T.unpack txt)
            Just fun -> pure (GlobalToken (ParsedPrim fun))
        uuidRef = do
          void (Mega.string "uuid:")
          txt <- uuidLexer
          pure (GlobalToken (ParsedUuid txt))
        sha512Ref = do
          void (Mega.string "sha512:")
          txt <- Mega.takeWhile1P Nothing isAlphaNum
          case sha512HexParser txt of
            Left e -> fail ("Invalid SHA512 hash: " ++ e)
            Right sha -> pure (GlobalToken (ParsedHash (Hash sha)))
    string =
      located
        (do void (Mega.char '"')
            contents <- Mega.manyTill Lexer.charLiteral (Mega.char '"')
            pure (StringToken (T.pack contents)))
    camelWord =
      located
        (do c <- Mega.takeWhile1P Nothing ((&&) <$> isAlpha <*> isLower)
            cs <-
              Mega.takeWhileP Nothing ((||) <$> isAlphaNum <*> flip elem ['_'])
            let text = (c <> cs)
            case text of
              "let" -> pure LetToken
              "in" -> pure InToken
              _ -> pure (CamelCaseToken text))
    anyWord =
      located
        (do c <- Mega.takeWhile1P Nothing isAlpha
            cs <-
              Mega.takeWhileP Nothing ((||) <$> isAlphaNum <*> flip elem ['_'])
            pure (AnyWordToken (c <> cs)))
    integer =
      Mega.try
        (located (NaturalToken <$> Lexer.decimal) <*
         Mega.notFollowedBy (void (Mega.char '.')))
    decimal =
      located
        (do num <- Mega.takeWhile1P (pure "digit") isDigit
            void (Mega.char '.')
            denom <- Mega.takeWhile1P (pure "digit") isDigit
            case T.decimal (num <> denom) of
              Right (i, "") ->
                pure
                  (DecimalToken
                     Decimal
                       {places = fromIntegral (T.length denom), integer = i})
              _ -> fail "Invalid decimal.")
    symbol =
      located
        (Mega.choice
           [ HoleToken <$ Mega.char '_'
           , BarToken <$ Mega.char '|'
           , HashToken <$ Mega.char '#'
           , QuestionToken <$ Mega.char '?'
           , OpenSquareToken <$ Mega.char '['
           , CloseSquareToken <$ Mega.char ']'
           , OpenCurlyToken <$ Mega.char '{'
           , CloseCurlyToken <$ Mega.char '}'
           , OpenRoundToken <$ Mega.char '('
           , CloseRoundToken <$ Mega.char ')'
           , RightArrowToken <$ Mega.try (Mega.string "->")
           , OperatorToken <$> Mega.string "*"
           , OperatorToken <$> Mega.string "+"
           , OperatorToken <$> Mega.string "-"
           , OperatorToken <$> Mega.string ">="
           , OperatorToken <$> Mega.string "<="
           , OperatorToken <$> Mega.string "/="
           , OperatorToken <$> Mega.string "<"
           , OperatorToken <$> Mega.string ">"
           , OperatorToken <$> Mega.string "="
           , OperatorToken <$> Mega.string "/"
           , BackslashToken <$ Mega.char '\\'
           , DoubleColonToken <$ Mega.try (Mega.string "::")
           , SemiColonToken <$ Mega.try (Mega.string ";")
           , ColonToken <$ Mega.try (Mega.string ":")
           , CommaToken <$ Mega.try (Mega.char ',')
           , PeriodToken <$ Mega.try (Mega.char '.')
           ])

-- UUID consists of: 8-4-4-4-12 hexadecimal
uuidLexer :: Lexer Uuid
uuidLexer = do
  p1 <- Mega.takeP (Just "UUID component") 8
  dash_
  p2 <- Mega.takeP (Just "UUID component") 4
  dash_
  p3 <- Mega.takeP (Just "UUID component") 4
  dash_
  p4 <- Mega.takeP (Just "UUID component") 4
  dash_
  p5 <- Mega.takeP (Just "UUID component") 12
  let txt = T.concat [p1, "-", p2, "-", p3, "-", p4, "-", p5]
  case UUID.fromText txt of
    Nothing -> fail ("Invalid UUID: " <> T.unpack txt)
    Just uuid' -> pure (Uuid (UUID.toText uuid'))
  where
    dash_ = Mega.token (\char -> guard (char == '-')) mempty

-- | Retain location information for a token.
located :: Mega.MonadParsec e s m => m Token -> m (Located Token)
located m = do
  start <- Mega.getSourcePos
  thing <- m
  end <- Mega.getSourcePos
  pure
    (Located
       { location =
           SourceLocation
             { end =
                 SourcePos
                   { line = Mega.unPos (Mega.sourceLine end)
                   , column = Mega.unPos (Mega.sourceColumn end)
                   , name = Mega.sourceName end
                   }
             , start =
                 SourcePos
                   { line = Mega.unPos (Mega.sourceLine start)
                   , column = Mega.unPos (Mega.sourceColumn start)
                   , name = Mega.sourceName start
                   }
             }
       , thing
       })

prims :: Map Text Function
prims =
  M.fromList
    [ ("array_map", MapFunction)
    , ("array_filter", FilterFunction)
    , ("array_length", LengthFunction)
    , ("array_null", NullFunction)
    , ("vega_raw", VegaFunction)
    , ("array_sum", SumFunction)
    , ("array_average", AverageFunction)
    , ("array_distinct", DistinctFunction)
    , ("array_minimum", MinimumFunction)
    , ("array_maximum", MaximumFunction)
    , ("array_sort", SortFunction)
    , ("array_concat", ConcatFunction)
    , ("array_find", FindFunction)
    , ("array_any", AnyFunction)
    , ("array_all", AllFunction)
    , ("from_ok", FromOkFunction)
    ]

$(makePrisms ''Token)
