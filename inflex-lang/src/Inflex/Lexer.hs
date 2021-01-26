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
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Void
import           GHC.Generics
import           Inflex.Types
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

--------------------------------------------------------------------------------
-- Lexer

-- | Lex unquoted regular code e.g. @let x = 1@.
tokensLexer :: Lexer (Seq (Located Token))
tokensLexer = fmap Seq.fromList (Mega.many tokenLexer)

-- | Lex a single token.
tokenLexer :: Lexer (Located Token)
tokenLexer =
  Mega.choice
    [  string
    ,  camelWord
    ,  anyWord
    ,  symbol
    ,  integer
    ,  decimal
    ] <*
  Mega.space
  where
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

$(makePrisms ''Token)
