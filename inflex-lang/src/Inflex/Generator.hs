{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Type generator for Inflex.

module Inflex.Generator
  ( generateText
  , RenameGenerateError(..)
  ) where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Text (Text)
import Inflex.Renamer
import Inflex.Types

--------------------------------------------------------------------------------
-- Types

newtype Generate a = Generate { runGenerator :: Identity a}
  deriving (Functor, Applicative)

data RenameGenerateError
  = RenameGenerateError ParseRenameError
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level

generateText :: FilePath -> Text -> Either RenameGenerateError (Expression Generated)
generateText fp text = do
  expression <- first RenameGenerateError (renameText fp text)
  pure (runIdentity (runGenerator (expressionGenerator expression)))

--------------------------------------------------------------------------------
-- Generators

expressionGenerator :: Expression Renamed -> Generate (Expression Generated)
expressionGenerator =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (literalGenerator literal)

literalGenerator :: Literal Renamed -> Generate (Literal Generated)
literalGenerator =
  \case
    IntegerLiteral integery -> fmap IntegerLiteral (integeryGenerator integery)

integeryGenerator :: Integery Renamed -> Generate (Integery Generated)
integeryGenerator Integery {..} = pure Integery {..}
