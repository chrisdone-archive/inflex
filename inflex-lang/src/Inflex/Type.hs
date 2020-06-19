{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
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
    GlobalExpression global -> globalType global

globalType :: Global s -> StagedType s
globalType Global {scheme} =
  case scheme of
    ParsedScheme -> ()
    RenamedScheme -> ()
    GeneratedScheme scheme' -> schemeType scheme'
    SolvedScheme scheme' -> schemeType scheme'
    GeneralisedScheme scheme' -> schemeType scheme'
    ResolvedScheme scheme' -> schemeType scheme'

schemeType :: Scheme s -> StagedType s
schemeType Scheme{typ} = typ

lambdaType :: Lambda s -> StagedType s
lambdaType Lambda {typ} = typ

applyType :: Apply s -> StagedType s
applyType Apply {typ} = typ

variableType :: Variable s -> StagedType s
variableType Variable {typ} = typ

literalType :: Literal s -> StagedType s
literalType =
  \case
    NumberLiteral number -> numberType number

numberType :: Number s -> StagedType s
numberType Number {typ} = typ

paramType :: Param s -> StagedType s
paramType Param {typ} = typ
