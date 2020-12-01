{-# LANGUAGE OverloadedStrings #-}
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

typeInput :: Type s -> Type s
typeInput =
  \case
    ApplyType TypeApplication {function = ApplyType TypeApplication { function = ConstantType TypeConstant {name = FunctionTypeName}
                                                                    , argument = input
                                                                    }} -> input
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
    HoleExpression hole -> holeType hole
    VariantExpression variant -> variantType variant

recordType :: Record s -> StagedType s
recordType Record {typ} = typ

arrayType :: Array s -> StagedType s
arrayType Array {typ} = typ

propType :: Prop s -> StagedType s
propType Prop {typ} = typ

holeType :: Hole s -> StagedType s
holeType Hole {typ} = typ

variantType :: Variant s -> StagedType s
variantType Variant {typ} = typ

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
    EqualIntegerInstance -> integerT .-> integerT .-> boolType BuiltIn
    EqualTextInstance -> textT .-> textT .-> boolType BuiltIn
    EqualDecimalInstance nat ->
      decimalT nat .-> decimalT nat .-> boolType BuiltIn
    CompareIntegerInstance -> integerT .-> integerT .-> boolType BuiltIn
    CompareTextInstance -> textT .-> textT .-> boolType BuiltIn
    CompareDecimalInstance nat ->
      decimalT nat .-> decimalT nat .-> boolType BuiltIn

--------------------------------------------------------------------------------
-- Convenience DSL for built-in types

nullType :: StagedLocation s -> Type s
nullType location =
  RowType TypeRow {location, typeVariable = Nothing, fields = []}

boolType :: StagedLocation s -> Type s
boolType location =
  VariantType
    (RowType
       (TypeRow
          { location
          , typeVariable = Nothing
          , fields =
              [ Field {location, name = "true", typ = nullType location}
              , Field {location, name = "false", typ = nullType location}
              ]
          }))

boolT :: Type Polymorphic
boolT = boolType BuiltIn

maybeType :: StagedLocation s -> Type s -> Type s
maybeType location a =
  VariantType
    (RowType
       (TypeRow
          { location
          , typeVariable = Nothing
          , fields =
              [ Field {location, name = "some", typ = a}
              , Field {location, name = "none", typ = nullType location}
              ]
          }))

integerT :: (StagedLocation s ~ Cursor) => Type s
integerT =
  ConstantType
    TypeConstant {location = BuiltIn, name = IntegerTypeName}

textT :: (StagedLocation s ~ Cursor) => Type s
textT =
  ConstantType
    TypeConstant {location = BuiltIn, name = TextTypeName}

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

decimalTVar :: (StagedLocation s ~ Cursor) => Type s -> Type s
decimalTVar nat =
  ApplyType
    TypeApplication
      { function =
          ConstantType TypeConstant {location = BuiltIn, name = DecimalTypeName}
      , argument = nat
      , location = BuiltIn
      , kind = TypeKind
      }


infixr .->
(.->) :: (StagedLocation s ~ Cursor) => Type s -> Type s -> Type s
(.->) i o =
  ApplyType
    TypeApplication
      { function =
          ApplyType
            TypeApplication
              { function =
                  ConstantType
                    TypeConstant {location = BuiltIn, name = FunctionTypeName}
              , argument = i
              , location = BuiltIn
              , kind = FunKind TypeKind TypeKind
              }
      , argument = o
      , location = BuiltIn
      , kind = TypeKind
      }

--------------------------------------------------------------------------------
-- Schemes

functionOutput :: Function -> Type Polymorphic
functionOutput func =
  let Scheme {typ} = functionScheme BuiltIn func
   in typeOutput typ

functionScheme :: Cursor -> Function -> Scheme Polymorphic
functionScheme location =
  \case
    MapFunction -> mono ((a .-> b) .-> ArrayType a .-> ArrayType b)
    FilterFunction -> mono ((a .-> boolT) .-> ArrayType a .-> ArrayType a)
    DistinctFunction -> poly [comparable a] (ArrayType a .-> ArrayType a)
    SortFunction -> poly [comparable a] (ArrayType a .-> ArrayType a)
    AndFunction -> mono (ArrayType boolT .-> boolT)
    OrFunction -> mono (ArrayType boolT .-> integerT)
    SumFunction -> mono (ArrayType a .-> a)
    MinimumFunction -> mono (ArrayType a .-> a)
    MaximumFunction -> mono (ArrayType a .-> a)
    AverageFunction -> mono (ArrayType a .-> a)
    LengthFunction -> mono (ArrayType a .-> integerT)
    NullFunction -> mono (ArrayType a .-> boolT)
    FindFunction -> mono ((a .-> boolT) .-> ArrayType a .-> maybeType location a)
    LookupFunction -> undefined
    AllFunction -> mono ((a .-> boolT) .-> ArrayType a .-> boolT)
    AnyFunction -> mono ((a .-> boolT) .-> ArrayType a .-> boolT)
  where
    mono t = Scheme {location, constraints = [], typ = t}
    poly p t = Scheme {location, constraints = p, typ = t}
    comparable t = ClassConstraint {className = CompareClassName, typ = pure t, location}
    a = typeVariable 0
    b = typeVariable 1
    typeVariable index =
      VariableType
        (TypeVariable
           {location = (), prefix = (), index = index, kind = TypeKind})
