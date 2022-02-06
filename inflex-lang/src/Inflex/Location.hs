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
    RecordExpression record -> recordLocation record
    PropExpression prop -> propLocation prop
    HoleExpression hole -> holeLocation hole
    VariantExpression variant -> variantLocation variant
    ArrayExpression array -> arrayLocation array
    LambdaExpression lambda -> lambdaLocation lambda
    CaseExpression case' -> caseLocation case'
    InfixExpression infix' -> infixLocation infix'
    GlobalExpression global -> globalLocation global
    ApplyExpression apply -> applyLocation apply
    VariableExpression variable -> variableLocation variable
    CellRefExpression ref -> cellRefLocation ref

cellRefLocation :: CellRef s -> StagedLocation s
cellRefLocation CellRef {location} = location

lambdaLocation :: Lambda s -> StagedLocation s
lambdaLocation Lambda {location} = location

recordLocation :: Record s -> StagedLocation s
recordLocation Record {location} = location

propLocation :: Prop s -> StagedLocation s
propLocation Prop {location} = location

holeLocation :: Hole s -> StagedLocation s
holeLocation Hole {location} = location

variantLocation :: Variant s -> StagedLocation s
variantLocation Variant {location} = location

arrayLocation :: Array s -> StagedLocation s
arrayLocation Array {location} = location

paramLocation :: Param s -> StagedLocation s
paramLocation Param {location} = location

caseLocation :: Case s -> StagedLocation s
caseLocation Case {location} = location

infixLocation :: Infix s -> StagedLocation s
infixLocation Infix {location} = location

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
    TextLiteral LiteralText{location} -> location

numberLocation :: Number s -> StagedLocation s
numberLocation Number {location} = location
