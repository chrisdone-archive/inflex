{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Fill in globals.

module Inflex.Filler where

import qualified Data.Map.Strict as M
import           Data.Validation
import           Inflex.Types
import           Inflex.Types.Filler

--------------------------------------------------------------------------------
-- Top-level entry points

expressionFill ::
     FillerEnv e
  -> Expression Renamed
  -> Filler e (Expression Filled)
expressionFill globals =
  \case
    RecordExpression record -> fmap RecordExpression (recordFill globals record)
    CaseExpression case' -> fmap CaseExpression (caseFill globals case')
    FoldExpression fold' -> fmap FoldExpression (foldFill globals fold')
    UnfoldExpression unfold' -> fmap UnfoldExpression (unfoldFill globals unfold')
    IfExpression if' -> fmap IfExpression (ifFill globals if')
    PropExpression prop -> fmap PropExpression (propFill globals prop)
    HoleExpression hole -> pure (HoleExpression (holeFill hole))
    ArrayExpression array -> fmap ArrayExpression (arrayFill globals array)
    VariantExpression variant -> fmap VariantExpression (variantFill globals variant)
    LiteralExpression literal -> pure (LiteralExpression (literalFill literal))
    LambdaExpression lambda -> fmap LambdaExpression (lambdaFill globals lambda)
    LetExpression let' -> fmap LetExpression (letFill globals let')
    InfixExpression infix' -> fmap InfixExpression (infixFill globals infix')
    ApplyExpression apply -> fmap ApplyExpression (applyFill globals apply)
    VariableExpression variable ->
      pure (VariableExpression (variableFill variable))
    GlobalExpression global -> fmap GlobalExpression (globalFill globals global)

--------------------------------------------------------------------------------
-- Fillers

propFill :: FillerEnv e -> Prop Renamed -> Filler e (Prop Filled)
propFill globals Prop {..} = do
  expression' <- expressionFill globals expression
  pure Prop {expression = expression', ..}

arrayFill :: FillerEnv e -> Array Renamed -> Filler e (Array Filled)
arrayFill globals Array {..} = do
  expressions' <- traverse (expressionFill globals) expressions
  pure Array {expressions = expressions', ..}

variantFill :: FillerEnv e -> Variant Renamed -> Filler e (Variant Filled)
variantFill globals Variant {..} = do
  argument' <- traverse (expressionFill globals) argument
  pure Variant {argument = argument', ..}

recordFill :: FillerEnv e -> Record Renamed -> Filler e (Record Filled)
recordFill globals Record {..} = do
  fields' <- traverse fieldFill fields
  pure Record {fields = fields', ..}
  where
    fieldFill FieldE {location = l, ..} = do
      expression' <- expressionFill globals expression
      pure FieldE {expression = expression', location = l, ..}

caseFill :: FillerEnv e -> Case Renamed -> Filler e (Case Filled)
caseFill globals Case {..} = do
  scrutinee' <- expressionFill globals scrutinee
  alternatives' <- traverse (alternativeFill globals) alternatives
  pure Case {alternatives = alternatives', scrutinee = scrutinee', ..}

foldFill :: FillerEnv e -> Fold Renamed -> Filler e (Fold Filled)
foldFill globals Fold {..} = do
  expression' <- expressionFill globals expression
  pure Fold {expression = expression', ..}

unfoldFill :: FillerEnv e -> Unfold Renamed -> Filler e (Unfold Filled)
unfoldFill globals Unfold {..} = do
  expression' <- expressionFill globals expression
  pure Unfold {expression = expression', ..}

alternativeFill ::
     FillerEnv e
  -> Alternative Renamed
  -> Filler e (Alternative Filled)
alternativeFill globals Alternative {location = l, ..} = do
  let pattern'' = patternFill pattern'
  expression' <- expressionFill globals expression
  pure
    Alternative
      {expression = expression', location = l, pattern' = pattern'', ..}
  where
    patternFill :: Pattern Renamed -> Pattern Filled
    patternFill =
      \case
        ParamPattern Param {..} -> ParamPattern Param {..}
        VariantPattern VariantP {argument, ..} ->
          VariantPattern VariantP {argument = fmap paramFill argument, ..}

globalFill :: FillerEnv e -> Global Renamed -> Filler e (Global Filled)
globalFill env@FillerEnv {namesTohash, uuidsToHash} Global {..} = do
  case name of
    UnresolvedGlobalText textName ->
      Filler (Failure (pure (MissingGlobal env textName)))
    UnresolvedUuid uuid ->
      case M.lookup uuid uuidsToHash of
        Nothing -> Filler (Failure (pure (MissingGlobalUuid env uuid)))
        Just result -> do
          case result of
            Left e -> Filler (Failure (pure (OtherCellUuidError uuid e)))
            Right globalRef ->
              pure
                Global {name = HashGlobal globalRef, scheme = FilledScheme, ..}
    ResolvedGlobalRef textName globalRef ->
      case M.lookup textName namesTohash of
        Nothing -> pure Global {scheme = FilledScheme, name = globalRef, ..}
        Just result -> do
          case result of
            Left e -> Filler (Failure (pure (OtherCellError textName e)))
            Right globalRef' ->
              pure
                Global {name = HashGlobal globalRef', scheme = FilledScheme, ..}
    ExactGlobalRef globalRef ->
      pure Global {scheme = FilledScheme, name = globalRef, ..}

lambdaFill ::
     FillerEnv e
  -> Lambda Renamed
  -> Filler e (Lambda Filled)
lambdaFill globals Lambda {..} = do
  body' <- expressionFill globals body
  pure Lambda {body = body', param = paramFill param, ..}

letFill ::
     FillerEnv e
  -> Let Renamed
  -> Filler e (Let Filled)
letFill globals Let {..} = do
  binds' <- traverse (bindFill globals) binds
  body' <- expressionFill globals body
  pure Let {binds = binds', body = body', ..}

infixFill ::
     FillerEnv e
  -> Infix Renamed
  -> Filler e (Infix Filled)
infixFill globals Infix {..} = do
  left' <- expressionFill globals left
  right' <- expressionFill globals right
  global' <- globalFill globals global
  pure Infix {left = left', right = right', global = global', ..}

bindFill ::
     FillerEnv e
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
     FillerEnv e
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

ifFill ::
     FillerEnv e
  -> If Renamed
  -> Filler e (If Filled)
ifFill globals If {..} = do
  condition' <- expressionFill globals condition
  consequent' <- expressionFill globals consequent
  alternative' <- expressionFill globals alternative
  pure If
    { condition = condition'
    , consequent = consequent'
    , alternative = alternative'
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
    TextLiteral LiteralText {..} ->
      TextLiteral LiteralText {..}

numberFill ::
     Number Renamed
  -> Number Filled
numberFill Number {..} = Number {..}

paramFill ::
     Param Renamed
  -> Param Filled
paramFill Param {..} = Param {..}

holeFill ::
     Hole Renamed
  -> Hole Filled
holeFill Hole {..} = Hole {..}
