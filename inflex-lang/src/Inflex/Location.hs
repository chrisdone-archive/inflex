{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Inflex.Location where

import Inflex.Types

expressionLocation :: Expression s -> StagedLocation s
expressionLocation =
  \case
    LiteralExpression literal -> literalLocation literal
    LambdaExpression lambda -> lambdaLocation lambda
    ApplyExpression apply -> applyLocation apply
    VariableExpression variable -> variableLocation variable

lambdaLocation :: Lambda s -> StagedLocation s
lambdaLocation Lambda {location} = location

applyLocation :: Apply s -> StagedLocation s
applyLocation Apply {location} = location

variableLocation :: Variable s -> StagedLocation s
variableLocation Variable {location} = location

literalLocation :: Literal s -> StagedLocation s
literalLocation =
  \case
    IntegerLiteral integery -> integeryLocation integery

integeryLocation :: Integery s -> StagedLocation s
integeryLocation Integery {location} = location
