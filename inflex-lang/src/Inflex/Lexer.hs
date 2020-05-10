{-# LANGUAGE InstanceSigs #-}
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
  , lexText
  , satisfy
  ) where

import           Data.Char
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Data.Void
import           GHC.Generics
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Text.Megaparsec.Error

--------------------------------------------------------------------------------
-- Types

-- | Lex text into a series of Tokens.
type Lexer = Mega.Parsec Void Text

-- | Parser of said tokens.
type Parser = Mega.Parsec Void (Seq (Located Token))

-- | Lexical tokens for the Inflex language.
data Token
  = LowerWordToken !Text
  | OpenSquareToken
  | CloseSquareToken
  | OpenRoundToken
  | CloseRoundToken
  | IntegerToken !Integer
  deriving (Show, Eq, Ord, Generic)

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
  , name :: FilePath
  } deriving (Show, Eq, Ord, Generic)

-- | This instance gives support to parse LTokens with megaparsec.
instance Mega.Stream (Seq (Located Token)) where
  type Token (Seq (Located Token)) = Located Token
  type Tokens (Seq (Located Token)) = Seq (Located Token)
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = Seq.fromList
  chunkToTokens Proxy = toList
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  positionAt1 Proxy _ (Located start _ _) = toSourcePos start
  positionAtN Proxy pos Seq.Empty = pos
  positionAtN Proxy _ (Located start _ _ Seq.:<| _) = toSourcePos start
  advance1 Proxy _ _ (Located _ end _) = toSourcePos end
  advanceN Proxy _ pos Seq.Empty = pos
  advanceN Proxy _ _ ts =
    let Located _ end _ = last (toList ts)
     in toSourcePos end
  take1_ Seq.Empty = Nothing
  take1_ (t Seq.:<| ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just (mempty, s)
    | null s = Nothing
    | otherwise = Just (Seq.splitAt n s)
  takeWhile_ = Seq.spanl

instance Mega.ShowToken (Located Token) where
  showTokens = unwords . map show . toList

--------------------------------------------------------------------------------
-- Entry points

-- | Lex a given block of text.
lexText :: FilePath -> Text -> Either (ParseError (Mega.Token Text) Void) (Seq (Located Token))
lexText fp bs =
  Mega.runParser (Mega.space *> tokensLexer <* Mega.eof) fp bs

--------------------------------------------------------------------------------
-- Lexer

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
  start <- Mega.getPosition
  thing <- m
  end <- Mega.getPosition
  pure
    (Located
       { end =
           SourcePos
             { line = Mega.unPos (Mega.sourceLine end)
             , column = Mega.unPos (Mega.sourceLine end)
             , name = Mega.sourceName end
             }
       , start =
           SourcePos
             { line = Mega.unPos (Mega.sourceLine start)
             , column = Mega.unPos (Mega.sourceLine start)
             , name = Mega.sourceName start
             }
       , thing
       })

toSourcePos :: SourcePos -> Mega.SourcePos
toSourcePos SourcePos {line, column, name} =
  Mega.SourcePos
    { Mega.sourceName = name
    , Mega.sourceLine = Mega.mkPos line
    , Mega.sourceColumn = Mega.mkPos column
    }

--------------------------------------------------------------------------------
-- Parseable tokens support

satisfy :: (Token -> Maybe a) -> Parser (Located a)
satisfy f =
  Mega.token
    (\case
       l@(Located {thing = tok})
         | Just token <- f tok -> Right (fmap (const token) l)
       l -> Left (Just (Mega.Tokens (NE.fromList [l])), mempty))
    Nothing
