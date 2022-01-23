{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

-- | Types of AST nodes.

module Inflex.Type where

import           Data.Text (Text)
import           Inflex.Types
import           Numeric.Natural

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

-- One layer function output.
funOutput1 :: Type s -> Type s
funOutput1 =
  \case
    ApplyType TypeApplication { function = ApplyType TypeApplication {function = ConstantType TypeConstant {name = FunctionTypeName}}
                              , argument = output
                              } -> output
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
    CaseExpression case' -> caseType case'
    LiteralExpression literal -> literalType literal
    ArrayExpression array -> arrayType array
    LambdaExpression lambda -> lambdaType lambda
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

caseType :: Case s -> StagedType s
caseType Case {typ} = typ

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
  RecordType (RowType TypeRow {location, typeVariable = Nothing, fields = []})

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

maybeType :: [Text] -> StagedLocation s -> Type s -> Type s
maybeType alts location a =
  VariantType
    (RowType
       (TypeRow
          { location
          , typeVariable = Nothing
          , fields =
              (Field {location, name = "ok", typ = a} :
               [ Field {location, name = FieldName name, typ = nullType location}
               | name <- alts
               ])
          }))

okishType :: StagedLocation s -> StagedRowVariable s -> Type s -> Type s
okishType location variable a =
  VariantType
    (RowType
       (TypeRow
          { location
          , typeVariable = Just variable
          , fields =
              [ Field {location, name = "ok", typ = a}
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

vegaT :: (StagedLocation s ~ Cursor) => Type s
vegaT =
  ConstantType
    TypeConstant {location = BuiltIn, name = VegaTypeName}

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
    VegaFunction -> mono (a .-> vegaT)
    MapFunction -> mono ((a .-> b) .-> ArrayType a .-> ArrayType b)
    FilterFunction -> mono ((a .-> boolT) .-> ArrayType a .-> ArrayType a)
    SumFunction ->
      poly
        [addable a, frominteger a]
        (ArrayType a .-> maybeType ["sum_empty"] location a)
    LengthFunction -> poly [frominteger b] (ArrayType a .-> b)
    NullFunction -> mono (ArrayType a .-> boolT)
    NotFunction -> mono (boolT .-> boolT)
    AverageFunction ->
      poly
        [addable a, divisible a, frominteger a]
        (ArrayType a .-> maybeType ["average_empty"] location a)
    DistinctFunction -> poly [comparable a] (ArrayType a .-> ArrayType a)
    SortFunction -> poly [comparable a] (ArrayType a .-> ArrayType a)
    ConcatFunction -> poly [] (ArrayType (ArrayType a) .-> ArrayType a)
    AndFunction -> mono (ArrayType boolT .-> boolT)
    OrFunction -> mono (ArrayType boolT .-> integerT)
    MinimumFunction ->
      poly
        [comparable a]
        (ArrayType a .-> maybeType ["minimum_empty"] location a)
    MaximumFunction ->
      poly
        [comparable a]
        (ArrayType a .-> maybeType ["maximum_empty"] location a)
    FindFunction ->
      mono
        ((a .-> boolT) .-> ArrayType a .->
         maybeType ["find_empty", "find_failed"] location a)
    AllFunction ->
      mono
        ((a .-> boolT) .-> ArrayType a .->
         maybeType ["all_empty"] location boolT)
    AnyFunction ->
      mono
        ((a .-> boolT) .-> ArrayType a .->
         maybeType ["any_empty"] location boolT)
    FromOkFunction -> mono (b .-> okishType BuiltIn c b .-> b)
    ScanFunction -> mono (a .-> (a .-> e .-> a) .-> ArrayType e .-> ArrayType a)
    ReduceFunction -> mono (a .-> (a .-> e .-> a) .-> ArrayType e .-> a)
    AccumFunction ->
      mono
        (a .->
         (record [("state", a), ("item", e)] .->
          record [("state", a), ("item", d)]) .->
         ArrayType e .->
         record [("state", a), ("items", ArrayType d)])

    -- Rich text types
    --
    -- We take a very simple type structure, copied from
    -- ProseMirror. The explicit sum type approach just adds more
    -- work.
    --
    -- Produce a doc
    RichDoc -> mono (ArrayType (constant RichBlockTypeName) .-> constant RichDocTypeName)
    -- Produce blocks
    RichParagraph -> mono (ArrayType (constant RichInlineTypeName) .-> constant RichBlockTypeName)
    -- Produce inlines
    RichCell -> mono (a .-> constant RichInlineTypeName)
    RichSource -> mono (a .-> constant RichInlineTypeName)
    RichText -> mono (constant TextTypeName .-> constant RichInlineTypeName)
    -- Marks, can apply to any inline anywhere (even cell)
    RichBold -> mono (constant RichInlineTypeName .-> constant RichInlineTypeName)
    RichItalic -> mono (constant RichInlineTypeName .-> constant RichInlineTypeName)
    RichLink -> mono (constant TextTypeName .-> constant RichInlineTypeName .-> constant RichInlineTypeName)

  where
    constant nam = ConstantType TypeConstant { location, name = nam }
    mono t = Scheme {location, constraints = [], typ = t}
    poly p t = Scheme {location, constraints = p, typ = t}
    comparable t =
      ClassConstraint {className = CompareClassName, typ = pure t, location}
    addable t =
      ClassConstraint {className = AddOpClassName, typ = pure t, location}
    divisible t =
      ClassConstraint {className = DivideOpClassName, typ = pure t, location}
    frominteger t =
      ClassConstraint {className = FromIntegerClassName, typ = pure t, location}
    c = typeVariable' 2 RowKind
    a = typeVariable 0
    b = typeVariable 1
    d = typeVariable 3
    e = typeVariable 4
    record fs =
      RecordType
        (RowType
           TypeRow
             { location = BuiltIn
             , fields =
                 map
                   (\(name, typ) ->
                      Field {name = FieldName name, typ, location = BuiltIn})
                   fs
             , typeVariable = Nothing
             })
    typeVariable index = VariableType (typeVariable' index TypeKind)
    typeVariable' index k =
      TypeVariable {location = (), prefix = (), index = index, kind = k}

-- TODO: implement properly
binOpType :: NumericBinOp -> Type Generalised
binOpType _ = nullType BuiltIn

--------------------------------------------------------------------------------
-- Patterns

pattern IntegerType :: Type Generalised
pattern IntegerType <- ConstantType TypeConstant{name=IntegerTypeName}

pattern DecimalType :: Natural -> Type Generalised
pattern DecimalType nat <-
  ApplyType
    TypeApplication
      { function =
          ConstantType TypeConstant {location = BuiltIn, name = DecimalTypeName}
      , argument =
          ConstantType TypeConstant {location = BuiltIn, name = NatTypeName nat}
      , location = BuiltIn
      , kind = TypeKind
      }
