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
  , _IntegerToken
  , _DecimalToken
  , _BackslashToken
  , _RightArrowToken
  , _LowerWordToken
  , _OpenRoundToken
  , _CloseRoundToken
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Void
import           GHC.Generics
import           Inflex.Types
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
  = LowerWordToken !Text
  | OpenSquareToken
  | CloseSquareToken
  | OpenRoundToken
  | CloseRoundToken
  | IntegerToken !Integer
  | DecimalToken !Decimal
  | BackslashToken
  | RightArrowToken
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

--------------------------------------------------------------------------------
-- Lexer

-- | Lex unquoted regular code e.g. @let x = 1@.
tokensLexer :: Lexer (Seq (Located Token))
tokensLexer =
  fmap
    mconcat
    (Mega.many
       (Mega.choice
          [ fmap pure symbol
          , fmap pure integer
          , fmap pure decimal
          , fmap pure lowerWord
          ] <*
        Mega.space))
  where
    lowerWord =
      located
        (do c <- Mega.takeWhile1P Nothing isAlpha
            cs <- Mega.takeWhileP Nothing isAlpha
            pure (LowerWordToken (c <> cs)))
    integer =
      Mega.try
        (located (IntegerToken <$> Lexer.decimal) <*
         Mega.notFollowedBy (void (Mega.char '.')))
    decimal =
      located
        (do num <- Mega.takeWhile1P (pure "digit") isDigit
            void (Mega.char '.')
            denom <- Mega.takeWhile1P (pure "digit") isDigit
            case T.decimal (num <> denom) of
              Right (i, "") ->
                pure
                  (DecimalToken Decimal {places = T.length denom, integer = i})
              _ -> fail "Invalid decimal.")
    symbol =
      located
        (Mega.choice
           [ OpenSquareToken <$ Mega.char '['
           , CloseSquareToken <$ Mega.char ']'
           , OpenRoundToken <$ Mega.char '('
           , CloseRoundToken <$ Mega.char ')'
           , RightArrowToken <$ Mega.try (Mega.string "->")
           , BackslashToken <$ Mega.char '\\'
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
