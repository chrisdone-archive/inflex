{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Step the code.

module Inflex.Stepper where

import           Data.Bifunctor
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Inflex.Resolver
import           Inflex.Types

--------------------------------------------------------------------------------
-- Types

data ResolveStepError
  = ResolverErrored GeneraliseResolveError
  | StepError StepError
  deriving (Show, Eq)

data StepError =
  NotInScope Hash
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Main entry points

stepText ::
     Map Hash (Scheme Polymorphic)
  -> Map Hash (Expression Resolved)
  -> FilePath
  -> Text
  -> Either ResolveStepError [Expression Resolved]
stepText schemes values fp text = do
  IsResolved {thing} <- first ResolverErrored (resolveText schemes fp text)
  first StepError (stepExpression values thing)

--------------------------------------------------------------------------------
-- Steppers

stepExpression ::
     Map Hash (Expression Resolved)
  -> Expression Resolved
  -> Either StepError [Expression Resolved]
stepExpression _globals =
  \case
     e@LiteralExpression {} -> pure [e]
     e@LambdaExpression {} -> pure [e]
     e@ApplyExpression {} -> pure [e]
     e@VariableExpression {} -> pure [e]
     e@GlobalExpression {} -> pure [e]
     e@LetExpression {} -> pure [e]
     e@InfixExpression {} -> pure [e]
