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
  , lexText
  ) where

import           Data.Char
import           Data.Sequence (Seq())
import           Data.Text (Text)
import           Data.Void
import           GHC.Generics
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Text.Megaparsec.Error

--------------------------------------------------------------------------------
-- Types

-- | Lexical tokens for the Inflex language.
data Token
  = LowerWordToken !Text
  | OpenSquareToken
  | CloseSquareToken
  | OpenRoundToken
  | CloseRoundToken
  | IntegerToken !Integer
  deriving (Show, Eq, Ord, Generic)

-- | Lex text into a series of Tokens.
type Lexer = Mega.Parsec Void Text

-- | A located token.
data Located l = Located
  { start :: !SourcePos
  , end :: !SourcePos
  , thing :: !l
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- | Position in source.
data SourcePos = SourcePos
  { line :: Int
  , column :: Int
  } deriving (Show, Eq, Ord, Generic)

-- | Lex a given block of text.
lexText :: FilePath -> Text -> Either (ParseErrorBundle Text Void) (Seq (Located Token))
lexText fp bs =
  Mega.runParser (Mega.space *> tokensLexer <* Mega.eof) fp bs

-- | Lex unquoted regular code e.g. @let x = 1@.
tokensLexer :: Lexer (Seq (Located Token))
tokensLexer =
  fmap
    mconcat
    (Mega.some
       (Mega.choice [fmap pure symbol, fmap pure integer, fmap pure lowerWord] <*
        Mega.space))
  where
    lowerWord =
      located
        (do c <- Mega.takeWhile1P Nothing isAlpha
            cs <- Mega.takeWhileP Nothing isAlpha
            pure (LowerWordToken (c <> cs)))
    integer = located (IntegerToken <$> Lexer.decimal)
    symbol =
      located
        (Mega.choice
           [ OpenSquareToken <$ Mega.char '['
           , CloseSquareToken <$ Mega.char ']'
           , OpenRoundToken <$ Mega.char '('
           , CloseRoundToken <$ Mega.char ')'
           ])

-- | Retain location information for a token.
located :: Mega.MonadParsec e s m => m Token -> m (Located Token)
located m = do
  start <- Mega.getSourcePos
  thing <- m
  end <- Mega.getSourcePos
  pure
    (Located
       { end =
           SourcePos
             { line = Mega.unPos (Mega.sourceLine end)
             , column = Mega.unPos (Mega.sourceLine end)
             }
       , start =
           SourcePos
             { line = Mega.unPos (Mega.sourceLine start)
             , column = Mega.unPos (Mega.sourceLine start)
             }
       , thing
       })
