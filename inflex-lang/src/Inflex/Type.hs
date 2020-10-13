{-# LANGUAGE ScopedTypeVariables #-}
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
    ArrayExpression array -> arrayType array
    LambdaExpression lambda -> lambdaType lambda
    LetExpression let' -> letType let'
    InfixExpression infix' -> infixType infix'
    ApplyExpression apply -> applyType apply
    VariableExpression variable -> variableType variable
    GlobalExpression global -> globalType global
    RecordExpression record -> recordType record
    PropExpression prop -> propType prop

recordType :: Record s -> StagedType s
recordType Record {typ} = typ

arrayType :: Array s -> StagedType s
arrayType Array {typ} = typ

propType :: Prop s -> StagedType s
propType Prop {typ} = typ

globalType :: Global s -> StagedType s
globalType Global {scheme} =
  case scheme of
    ParsedScheme -> Nothing
    RenamedScheme -> Nothing
    FilledScheme -> Nothing
    GeneratedScheme scheme' -> schemeType scheme'
    SolvedScheme scheme' -> schemeType scheme'
    GeneralisedScheme scheme' -> schemeType scheme'
    ResolvedScheme typ -> typ

schemeType :: Scheme s -> StagedType s
schemeType Scheme{typ} = typ

lambdaType :: Lambda s -> StagedType s
lambdaType Lambda {typ} = typ

letType :: Let s -> StagedType s
letType Let {typ} = typ

infixType :: Infix s -> StagedType s
infixType Infix {typ} = typ

applyType :: Apply s -> StagedType s
applyType Apply {typ} = typ

variableType :: Variable s -> StagedType s
variableType Variable {typ} = typ

literalType :: Literal s -> StagedType s
literalType =
  \case
    NumberLiteral number -> numberType number
    TextLiteral LiteralText{typ} -> typ

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
    IntegerOpInstance (_op :: NumericBinOp) ->
      integerT .-> integerT .-> integerT
    DecimalOpInstance n (_op :: NumericBinOp) ->
      decimalT n .-> decimalT n .-> decimalT n

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

--------------------------------------------------------------------------------
-- Schemes

functionScheme :: Cursor -> Function -> Scheme Polymorphic
functionScheme location =
  \case
    MapFunction ->
      Scheme
        { location
        , constraints = []
        , typ = (a .-> b) .-> ArrayType a .-> ArrayType b
        }
  where
    a = typeVariable 0
    b = typeVariable 1
    typeVariable index =
      VariableType
        (TypeVariable
           {location = (), prefix = (), index = index, kind = TypeKind})
