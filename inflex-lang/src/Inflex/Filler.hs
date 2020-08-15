{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Fill in globals.

module Inflex.Filler where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Validation
import           Inflex.Types
import           Inflex.Types.Filler

--------------------------------------------------------------------------------
-- Top-level entry points

expressionFill ::
     Map Text Hash
  -> Expression Renamed
  -> Filler (Expression Filled)
expressionFill globals =
  \case
    LiteralExpression literal ->
      pure (LiteralExpression (literalFill literal))
    LambdaExpression lambda ->
      fmap LambdaExpression (lambdaFill globals lambda)
    LetExpression let' ->
      fmap LetExpression (letFill globals let')
    InfixExpression infix' ->
      fmap InfixExpression (infixFill globals infix')
    ApplyExpression apply ->
      fmap ApplyExpression (applyFill globals apply)
    VariableExpression variable ->
      pure (VariableExpression (variableFill variable))
    GlobalExpression global ->
      fmap GlobalExpression (globalFill globals global)

--------------------------------------------------------------------------------
-- Fillers

globalFill :: Map Text Hash -> Global Renamed -> Filler (Global Filled)
globalFill globals Global {..} = do
  case name of
    UnresolvedGlobal textName ->
      case M.lookup textName globals of
        Nothing -> Filler (Failure (pure (MissingGlobal globals textName)))
        Just globalRef -> do
          pure Global {name = HashGlobal globalRef, scheme = FilledScheme, ..}
    GlobalRef globalRef ->
      pure Global {scheme = FilledScheme, name = globalRef, ..}

lambdaFill ::
     Map Text Hash
  -> Lambda Renamed
  -> Filler (Lambda Filled)
lambdaFill globals Lambda {..} = do
  body' <- expressionFill globals body
  pure Lambda {body = body', param = paramFill param, ..}

letFill ::
     Map Text Hash
  -> Let Renamed
  -> Filler (Let Filled)
letFill globals Let {..} = do
  binds' <- traverse (bindFill globals) binds
  body' <- expressionFill globals body
  pure Let {binds = binds', body = body', ..}

infixFill ::
     Map Text Hash
  -> Infix Renamed
  -> Filler (Infix Filled)
infixFill globals Infix {..} = do
  left' <- expressionFill globals left
  right' <- expressionFill globals right
  global' <- globalFill globals global
  pure Infix {left = left', right = right', global = global', ..}

bindFill ::
     Map Text Hash
  -> Bind Renamed
  -> Filler (Bind Filled)
bindFill globals Bind {..} = do
  value' <- expressionFill globals value
  pure
    Bind
      { param = paramFill param
      , value = value'
      , ..
      }

applyFill ::
     Map Text Hash
  -> Apply Renamed
  -> Filler (Apply Filled)
applyFill globals Apply {..} = do
  function' <- expressionFill globals function
  argument' <- expressionFill globals argument
  pure Apply
    { function = function'
    , argument = argument'
    , ..
    }

variableFill ::
     Variable Renamed
  -> Variable Filled
variableFill Variable {..} = Variable {..}

literalFill ::
     Literal Renamed
  -> Literal Filled
literalFill =
  \case
    NumberLiteral number ->
      NumberLiteral (numberFill number)

numberFill ::
     Number Renamed
  -> Number Filled
numberFill Number {..} = Number {..}

paramFill ::
     Param Renamed
  -> Param Filled
paramFill Param {..} = Param {..}
