{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Types of AST nodes.

module Inflex.Type where

import Inflex.Types
import Numeric.Natural

-- | Return the final output type.
--
-- typeOutput(x -> t) = typeOutput(t), or else typeOutput(y)=y.
typeOutput :: Type s -> Type s
typeOutput =
  \case
    ApplyType TypeApplication { function = ApplyType TypeApplication {function = ConstantType TypeConstant {name = FunctionTypeName}}
                              , argument = output
                              } -> typeOutput output
    t -> t

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
    ResolvedScheme typ -> typ

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

instanceNameType :: (StagedLocation s ~ Cursor) => InstanceName -> Type s
instanceNameType =
  \case
    FromIntegerIntegerInstance -> integerT .-> integerT
    FromIntegerDecimalInstance nat -> integerT .-> decimalT nat
    FromDecimalDecimalInstance FromDecimalInstance { supersetPlaces
                                                   , subsetPlaces
                                                   } ->
      decimalT subsetPlaces .-> decimalT supersetPlaces

--------------------------------------------------------------------------------
-- Convenience DSL for built-in types

integerT :: (StagedLocation s ~ Cursor) => Type s
integerT =
  ConstantType
    TypeConstant {location = BuiltIn, name = IntegerTypeName}

decimalT :: (StagedLocation s ~ Cursor) => Natural -> Type s
decimalT nat =
  ApplyType
    TypeApplication
      { function =
          ConstantType TypeConstant {location = BuiltIn, name = DecimalTypeName}
      , argument =
          ConstantType TypeConstant {location = BuiltIn, name = NatTypeName nat}
      , location = BuiltIn
      , kind = TypeKind
      }

(.->) :: (StagedLocation s ~ Cursor) => Type s -> Type s -> Type s
(.->) f x =
  ApplyType
    TypeApplication
      {function = f, argument = x, location = BuiltIn, kind = TypeKind}
