{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Types of AST nodes.

module Inflex.Type where

import Inflex.Types

expressionType :: Expression s -> StagedType s
expressionType =
  \case
    LiteralExpression literal -> literalType literal
    LambdaExpression lambda -> lambdaType lambda
    ApplyExpression apply -> applyType apply
    VariableExpression variable -> variableType variable

lambdaType :: Lambda s -> StagedType s
lambdaType Lambda {typ} = typ

applyType :: Apply s -> StagedType s
applyType Apply {typ} = typ

variableType :: Variable s -> StagedType s
variableType Variable {typ} = typ

literalType :: Literal s -> StagedType s
literalType =
  \case
    IntegerLiteral integery -> integeryType integery

integeryType :: Integery s -> StagedType s
integeryType Integery {typ} = typ

paramType :: Param s -> StagedType s
paramType Param {typ} = typ
