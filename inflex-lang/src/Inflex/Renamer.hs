{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Renamer for Inflex language.

module Inflex.Renamer
  ( renameText
  , RenameError(..)
  , ParseRenameError(..)
  ) where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Validation
import Inflex.Parser
import Inflex.Types

--------------------------------------------------------------------------------
-- Renamer types

data RenameError = RenameError
  deriving (Show, Eq)

newtype Renamer a = Renamer
  { runRenamer :: Validation (NonEmpty RenameError) a
  } deriving (Functor, Applicative)

data ParseRenameError
  = RenamerErrors (NonEmpty RenameError)
  | ParserErrored RenameParseError
  deriving (Show, Eq)

type CursorBuilder = Cursor -> Cursor

--------------------------------------------------------------------------------
-- Top-level

renameText :: FilePath -> Text -> Either ParseRenameError (Expression Renamed)
renameText fp text = do
  expression <- first ParserErrored (parseText fp text)
  first
    RenamerErrors
    (toEither (runRenamer (renameExpression id expression)))

--------------------------------------------------------------------------------
-- Renamers

renameExpression :: CursorBuilder -> Expression Parsed -> Renamer (Expression Renamed)
renameExpression cursor =
  \case
    LiteralExpression literal -> fmap LiteralExpression (renameLiteral cursor literal)
    LambdaExpression lambda -> fmap LambdaExpression (renameLambda (cursor . LambdaCursor) lambda)

renameLiteral :: CursorBuilder -> Literal Parsed -> Renamer (Literal Renamed)
renameLiteral cursor =
  \case
    IntegerLiteral integery -> fmap IntegerLiteral (renameIntegery cursor integery)

renameIntegery :: CursorBuilder -> Integery Parsed -> Renamer (Integery Renamed)
renameIntegery cursor Integery {..} = pure Integery {location = cursor FinalCursor, ..}

renameLambda :: CursorBuilder -> Lambda Parsed -> Renamer (Lambda Renamed)
renameLambda cursor Lambda {..} = do
  body' <- renameExpression cursor body
  pure Lambda {body = body', location = cursor FinalCursor, ..}
