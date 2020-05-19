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

--------------------------------------------------------------------------------
-- Top-level

renameText :: FilePath -> Text -> Either ParseRenameError (Expression Renamed)
renameText fp text = do
  expression <- first ParserErrored (parseText fp text)
  first RenamerErrors (toEither (runRenamer (renameExpression expression)))

--------------------------------------------------------------------------------
-- Renamers

renameExpression :: Expression Parsed -> Renamer (Expression Renamed)
renameExpression =
  \case
    LiteralExpression literal -> fmap LiteralExpression (renameLiteral literal)
    LambdaExpression lambda -> fmap LambdaExpression (renameLambda lambda)

renameLiteral :: Literal Parsed -> Renamer (Literal Renamed)
renameLiteral =
  \case
    IntegerLiteral integery -> fmap IntegerLiteral (renameIntegery integery)

renameIntegery :: Integery Parsed -> Renamer (Integery Renamed)
renameIntegery Integery {..} = pure Integery {..}

renameLambda :: Lambda Parsed -> Renamer (Lambda Renamed)
renameLambda Lambda {..} = do
  body' <- renameExpression body
  pure Lambda {body = body', ..}
