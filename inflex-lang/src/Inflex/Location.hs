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
    GlobalExpression global -> globalLocation global
    ApplyExpression apply -> applyLocation apply
    VariableExpression variable -> variableLocation variable

lambdaLocation :: Lambda s -> StagedLocation s
lambdaLocation Lambda {location} = location

globalLocation :: Global s -> StagedLocation s
globalLocation Global {location} = location

applyLocation :: Apply s -> StagedLocation s
applyLocation Apply {location} = location

variableLocation :: Variable s -> StagedLocation s
variableLocation Variable {location} = location

literalLocation :: Literal s -> StagedLocation s
literalLocation =
  \case
    NumberLiteral number -> numberLocation number

numberLocation :: Number s -> StagedLocation s
numberLocation Number {location} = location
