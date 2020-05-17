{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Type generator for Inflex.

module Inflex.Generator
  ( generateText
  , RenameGenerateError(..)
  ) where

import Control.Monad.State
import Data.Bifunctor
import Data.Text (Text)
import Inflex.Optics
import Inflex.Renamer
import Inflex.Types
import Optics

--------------------------------------------------------------------------------
-- Types

data GenerateState = GenerateState
  { counter :: !Integer
  } deriving (Show)

newtype Generate a = Generate
  { runGenerator :: State GenerateState a
  } deriving (Functor, Applicative, Monad)

data RenameGenerateError
  = RenameGenerateError ParseRenameError
  deriving (Show, Eq)

$(makeLensesWith (inflexRules ['counter]) ''GenerateState)

--------------------------------------------------------------------------------
-- Top-level

generateText :: FilePath -> Text -> Either RenameGenerateError (Expression Generated)
generateText fp text = do
  expression <- first RenameGenerateError (renameText fp text)
  pure
    (evalState
       (runGenerator (expressionGenerator expression))
       GenerateState {counter = 0})

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
integeryGenerator Integery {typ = _, ..} = do
  typ <- generateTypeVariable IntegeryPrefix
  pure Integery {typ, ..}

--------------------------------------------------------------------------------
-- Type system helpers

generateTypeVariable :: TypeVariablePrefix -> Generate GeneratedType
generateTypeVariable prefix =
  Generate
    (do i <- gets (view generateStateCounterL)
        modify' (over generateStateCounterL succ)
        pure (VariableGeneratedType prefix i))
