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
     Map Text (Either e Hash)
  -> Expression Renamed
  -> Filler e (Expression Filled)
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

globalFill :: Map Text (Either e Hash) -> Global Renamed -> Filler e (Global Filled)
globalFill globals Global {..} = do
  case name of
    UnresolvedGlobal textName ->
      case M.lookup textName globals of
        Nothing -> Filler (Failure (pure (MissingGlobal globals textName)))
        Just result -> do
          case result of
            Left e -> Filler (Failure (pure (OtherCellError textName e)))
            Right globalRef ->
              pure
                Global {name = HashGlobal globalRef, scheme = FilledScheme, ..}
    GlobalRef globalRef ->
      pure Global {scheme = FilledScheme, name = globalRef, ..}

lambdaFill ::
     Map Text (Either e Hash)
  -> Lambda Renamed
  -> Filler e (Lambda Filled)
lambdaFill globals Lambda {..} = do
  body' <- expressionFill globals body
  pure Lambda {body = body', param = paramFill param, ..}

letFill ::
     Map Text (Either e Hash)
  -> Let Renamed
  -> Filler e (Let Filled)
letFill globals Let {..} = do
  binds' <- traverse (bindFill globals) binds
  body' <- expressionFill globals body
  pure Let {binds = binds', body = body', ..}

infixFill ::
     Map Text (Either e Hash)
  -> Infix Renamed
  -> Filler e (Infix Filled)
infixFill globals Infix {..} = do
  left' <- expressionFill globals left
  right' <- expressionFill globals right
  global' <- globalFill globals global
  pure Infix {left = left', right = right', global = global', ..}

bindFill ::
     Map Text (Either e Hash)
  -> Bind Renamed
  -> Filler e (Bind Filled)
bindFill globals Bind {..} = do
  value' <- expressionFill globals value
  pure
    Bind
      { param = paramFill param
      , value = value'
      , ..
      }

applyFill ::
     Map Text (Either e Hash)
  -> Apply Renamed
  -> Filler e (Apply Filled)
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
