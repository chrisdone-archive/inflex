{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Type checker for normal form code.
--
-- The idea being that checking code that is in normal form has a much
-- faster algorithm for large arrays.
--
-- We should be able to jump from Renamed straight to Resolved in one
-- jump, with trivial unification.
--
-- The type given is polymorphic (i.e. has polytypes).
--
-- Normal form means: no if, no case, no globals (but that could
-- change, e.g. if globals are normal form), no lambdas, no
-- variables. Just atomics and lists, basically.
--
-- Because we need proper Cursor info, we do cursor generation here
-- rather than using the renamer.
--
-- We perform in two stages.
--
-- 1. Generate a type (elaboration) for the expression.
-- 2. Apply the type to the expression.
--
-- We have to do two steps because we don't know the full type until
-- the end (due to numbers).

module Inflex.NormalFormCheck
  ( resolveParsed
  , resolveParsedT
  , resolveParsedResolved
  , expressionGenerate
  , NormalFormCheckProblem(..)
  , T(..)
  ) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Coerce
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OM
import qualified Data.List as List
import           Data.Maybe
import           Data.Text (Text)
import           Data.Traversable
import           GHC.Generics
import           GHC.Natural
import           Inflex.Decimal
import           Inflex.Generator
import           Inflex.Parser2
import           Inflex.Types.Resolver
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types.Generator

--------------------------------------------------------------------------------
-- Types

data NormalFormCheckProblem
  = NotNormalForm
  | TypeMismatch !T !T
  | RecordFieldsMismatch [FieldName] [FieldName]
  | NoTypeSig
  | CouldntInternType (Type Parsed)
  deriving (Show, Eq, Generic)

data T
  = ArrayT !(Maybe T)
  | RecordT !(InsOrdHashMap FieldName T)
  | VariantT !(InsOrdHashMap TagName T)
  | IntegerT
  | DecimalT !Natural
  | TextT
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- REPL testing

_replText :: Text -> IO ()
_replText t =
  case parseText "repl" t of
    Left e -> error (show e)
    Right e ->
      case resolveParsed e of
        Left e' -> error (show e')
        Right a -> print a

--------------------------------------------------------------------------------
-- Top-level interface

-- | This function only works when an expression has an explicit type
-- signature, which has only monomorphic types. Also ensures that the
-- type sig matches the inferred type.
resolveParsed ::
     Expression Parsed -> Either NormalFormCheckProblem (Expression Resolved)
resolveParsed expression =
  case expressionType expression of
    Nothing -> Left NoTypeSig
    Just typ ->
      case toT typ of
        Nothing -> Left (CouldntInternType typ)
        Just sigT -> do
          inferredT <- expressionGenerate expression
          finalT <- oneWayUnifyT sigT inferredT
          apply expression (toTypeMono finalT)

-- | This function only works when an expression has an explicit type
-- signature, which has only monomorphic types. Also ensures that the
-- type sig matches the inferred type.
resolveParsedResolved ::
     Expression Parsed
  -> Either NormalFormCheckProblem (IsResolved (Expression Resolved))
resolveParsedResolved expression =
  case expressionType expression of
    Nothing -> Left NoTypeSig
    Just typ ->
      case toT typ of
        Nothing -> Left (CouldntInternType typ)
        Just sigT -> do
          inferredT <- expressionGenerate expression
          finalT <- oneWayUnifyT sigT inferredT
          thing <- apply expression (toTypeMono finalT)
          pure
            IsResolved
              { thing
              , scheme =
                  Scheme
                    { location = BuiltIn
                    , constraints = []
                    , typ = toTypePoly finalT
                    }
              , mappings = mempty
              }

-- | Same as 'resolveParsed', but returns the T.
resolveParsedT ::
     Expression Parsed -> Either NormalFormCheckProblem T
resolveParsedT expression =
  case expressionType expression of
    Nothing -> Left NoTypeSig
    Just typ ->
      case toT typ of
        Nothing -> Left (CouldntInternType typ)
        Just sigT -> do
          inferredT <- expressionGenerate expression
          finalT <- oneWayUnifyT sigT inferredT
          pure finalT

--------------------------------------------------------------------------------
-- Generation

expressionGenerate :: Expression Parsed -> Either NormalFormCheckProblem T
expressionGenerate =
  \case
    LiteralExpression literal -> pure $! (literalGenerator literal)
    ArrayExpression array -> arrayGenerate array
    RecordExpression record -> fmap RecordT (recordGenerate record)
    VariantExpression variant -> variantGenerate variant
    -- The rest of these are not normal form. We only consider the above cases.
    LambdaExpression {} -> Left NotNormalForm
    ApplyExpression {} -> Left NotNormalForm
    VariableExpression {} -> Left NotNormalForm
    GlobalExpression {} -> Left NotNormalForm
    LetExpression {} -> Left NotNormalForm
    InfixExpression {} -> Left NotNormalForm
    PropExpression {} -> Left NotNormalForm
    HoleExpression {} -> Left NotNormalForm
    IfExpression {} -> Left NotNormalForm
    CaseExpression {} -> Left NotNormalForm
    EarlyExpression {} -> Left NotNormalForm
    BoundaryExpression {} -> Left NotNormalForm

recordGenerate ::
     Record Parsed -> Either NormalFormCheckProblem (InsOrdHashMap FieldName T)
recordGenerate Record {fields} =
  fmap
    OM.fromList
    (traverse
       (\FieldE {name, expression} -> do
          t <- expressionGenerate expression
          pure (name, t))
       fields)

variantGenerate ::
     Variant Parsed -> Either NormalFormCheckProblem T
variantGenerate Variant {tag, argument} = do
  mtyp <- for argument expressionGenerate
  pure (VariantT (OM.singleton tag (fromMaybe nullT mtyp)))

-- TODO: Parallelism?
arrayGenerate :: Array Parsed -> Either NormalFormCheckProblem T
arrayGenerate Array {expressions} =
  foldM
    (\prev expression -> do
       next <- fmap (ArrayT . pure) (expressionGenerate expression)
       unifyT prev next)
    (ArrayT Nothing)
    expressions

literalGenerator :: Literal Parsed -> T
literalGenerator =
  \case
    NumberLiteral Number {number} -> someNumberType number
    TextLiteral {} -> TextT

someNumberType :: SomeNumber -> T
someNumberType =
  \case
    IntegerNumber {} -> IntegerT
    DecimalNumber Decimal {places} -> DecimalT places

nullT :: T
nullT = RecordT mempty

--------------------------------------------------------------------------------
-- Fast unification

unifyT :: T -> T -> Either NormalFormCheckProblem T
unifyT TextT TextT = pure TextT
unifyT IntegerT IntegerT = pure IntegerT
-- Arrays might be empty, and therefore without a type. Just take
-- whatever side has something.
unifyT (ArrayT Nothing) (ArrayT y) = pure (ArrayT y)
unifyT (ArrayT x) (ArrayT Nothing) = pure (ArrayT x)
unifyT (ArrayT (Just x)) (ArrayT (Just y)) = fmap (ArrayT . pure) (unifyT x y)
-- Records:
unifyT (RecordT x) (RecordT y) =
  if HM.keys (OM.toHashMap x) == HM.keys (OM.toHashMap y)
    then do
      !m <-
        fmap
          OM.fromList
          (traverse
             (\((k1, v1), v2) -> do
                t <- unifyT v1 v2
                pure (k1, t))
             (zip (HM.toList (OM.toHashMap x)) (HM.elems (OM.toHashMap y))))
      pure (RecordT m)
    else Left (RecordFieldsMismatch (OM.keys x) (OM.keys y))
-- Variants:
unifyT (VariantT x) (VariantT y) = do
  z <-
    sequence
      (OM.unionWith
         (\x' y' -> join (unifyT <$> x' <*> y'))
         (fmap pure x)
         (fmap pure y))
  pure (VariantT z)
-- Promotion of integer to decimal:
unifyT IntegerT (DecimalT n) = pure (DecimalT n)
unifyT (DecimalT n) IntegerT = pure (DecimalT n)
-- Promotion of smaller decimal to larger decimal:
unifyT (DecimalT x) (DecimalT y) = pure (DecimalT n)
  where !n = max x y
unifyT x y = Left (TypeMismatch x y)

--------------------------------------------------------------------------------
-- One-way unification

-- Left side is rigid.
oneWayUnifyT :: T -> T -> Either NormalFormCheckProblem T
oneWayUnifyT TextT TextT = pure TextT
oneWayUnifyT IntegerT IntegerT = pure IntegerT
-- Arrays might be empty, and therefore without a type. Just take
-- whatever side has something.
oneWayUnifyT (ArrayT Nothing) (ArrayT y) = pure (ArrayT y)
oneWayUnifyT (ArrayT x) (ArrayT Nothing) = pure (ArrayT x)
oneWayUnifyT (ArrayT (Just x)) (ArrayT (Just y)) = fmap (ArrayT . pure) (oneWayUnifyT x y)
-- Records:
oneWayUnifyT (RecordT x) (RecordT y) =
  if HM.keys (OM.toHashMap x) == HM.keys (OM.toHashMap y)
    then do
      !m <-
        fmap
          OM.fromList
          (traverse
             (\((k1, v1), v2) -> do
                t <- oneWayUnifyT v1 v2
                pure (k1, t))
             (zip (HM.toList (OM.toHashMap x)) (HM.elems (OM.toHashMap y))))
      pure (RecordT m)
    else Left (RecordFieldsMismatch (OM.keys x) (OM.keys y))
-- Variants:
oneWayUnifyT (VariantT x) (VariantT y) = do
  z <-
    sequence
      (OM.unionWith
         (\x' y' -> join (oneWayUnifyT <$> x' <*> y'))
         (fmap pure x)
         (fmap pure y))
  pure (VariantT z)
-- Promotion of integer to decimal:
oneWayUnifyT (DecimalT n) IntegerT = pure (DecimalT n)
-- Promotion of smaller decimal to larger decimal:
oneWayUnifyT (DecimalT x) (DecimalT y) | x >= y = pure (DecimalT x)
oneWayUnifyT x y = Left (TypeMismatch x y)

--------------------------------------------------------------------------------
-- Conversion to Real(tm) types

toTypeMono :: T -> Type Generalised
toTypeMono =
  flip evalState (GenerateState {counter = 0, equalityConstraints = mempty}) .
  go
  where
    go :: T -> State GenerateState (Type Generalised)
    go =
      \case
        IntegerT -> pure integerT
        DecimalT n -> pure (decimalT n)
        TextT -> pure textT
        ArrayT (Just t) -> fmap ArrayType (go t)
        ArrayT Nothing ->
          fmap
            ArrayType
            (generateVariableType BuiltIn ArrayElementPrefix TypeKind)
        VariantT fs -> do
          fs' <-
            traverse
              (\(TagName name, typ) -> do
                 typ' <- go typ
                 pure
                   Field
                     { location = BuiltIn
                     , name = FieldName name
                     , typ = typ'
                     })
              (OM.toList fs)
          var <- generateTypeVariable BuiltIn VariantRowVarPrefix RowKind
          pure
            (VariantType
               (RowType
                  TypeRow
                    {location = BuiltIn, typeVariable = Just var, fields = fs'}))
        RecordT fs -> do
          fs' <-
            traverse
              (\(name, typ) -> do
                 typ' <- go typ
                 pure Field {location = BuiltIn, name, typ = typ'})
              (OM.toList fs)
          pure
            (RecordType
               (RowType
                  TypeRow
                    {location = BuiltIn, typeVariable = Nothing, fields = fs'}))

toTypePoly :: T -> Type Polymorphic
toTypePoly =
  flip evalState (GenerateState {counter = 0, equalityConstraints = mempty}) .
  go
  where
    go :: T -> State GenerateState (Type Polymorphic)
    go =
      \case
        IntegerT -> pure integerT
        DecimalT n -> pure (decimalT n)
        TextT -> pure textT
        ArrayT (Just t) -> fmap ArrayType (go t)
        ArrayT Nothing ->
          fmap
            ArrayType
            (generateVariableType () () TypeKind)
        VariantT fs -> do
          fs' <-
            traverse
              (\(TagName name, typ) -> do
                 typ' <- go typ
                 pure
                   Field
                     { location = BuiltIn
                     , name = FieldName name
                     , typ = typ'
                     })
              (OM.toList fs)
          var <- generateTypeVariable () () RowKind
          pure
            (VariantType
               (RowType
                  TypeRow
                    {location = BuiltIn, typeVariable = Just var, fields = fs'}))
        RecordT fs -> do
          fs' <-
            traverse
              (\(name, typ) -> do
                 typ' <- go typ
                 pure Field {location = BuiltIn, name, typ = typ'})
              (OM.toList fs)
          pure
            (RecordType
               (RowType
                  TypeRow
                    {location = BuiltIn, typeVariable = Nothing, fields = fs'}))

--------------------------------------------------------------------------------
-- Application

--
-- Consideration: let's handle polymorphism LATER. Make monomorphic
-- types work first with a type sig. Then worry about generalization
-- later.
--

apply ::
     Expression Parsed
  -> Type Generalised
  -> Either NormalFormCheckProblem (Expression Resolved)
apply (LiteralExpression literal) typ =
  pure
    (LiteralExpression
       (case literal of
          NumberLiteral number -> NumberLiteral (increasePrecisionNumber number typ)
          TextLiteral text -> TextLiteral text {typ, location = BuiltIn}))
-- TODO: Parallelism?
apply (ArrayExpression array@Array {expressions}) (ArrayType typ) = do
  expressions' <- traverse (flip apply typ) expressions
  pure
    (ArrayExpression
       array
         {expressions = expressions', location = BuiltIn, typ = ArrayType typ})
-- TODO: Parallelism?
apply (RecordExpression record@Record {fields}) typ@(RecordType (RowType TypeRow {fields = types})) = do
  fields' <-
    traverse
      (\FieldE {expression, name, ..} -> do
         case List.find (\Field{name = name'} -> name == name') types of
           Nothing -> error "TODO: This is a bug."
           Just Field{typ=typ'} -> do
             expression' <- apply expression typ'
             pure FieldE {expression = expression', location = BuiltIn, ..})
      fields
  pure
    (RecordExpression
       record {fields = fields', typ, location = BuiltIn})
apply (VariantExpression variant@Variant {argument, tag}) typ@(VariantType (RowType TypeRow {fields = types})) = do
  argument' <-
    traverse
      (\expression -> do
         case List.find (\Field {name} -> tag == coerce name) types of
           Nothing -> error "TODO: This is a bug. [variant]"
           Just Field {typ = typ'} -> do
             expression' <- apply expression typ'
             pure expression')
      argument
  pure
    (VariantExpression variant {argument = argument', location = BuiltIn, typ})
apply _ _ = Left NotNormalForm

increasePrecisionNumber :: Number s -> Type Generalised -> Number Resolved
increasePrecisionNumber number@Number {number = someNumber} typ =
  number
    { typ
    , location = BuiltIn
    , number =
        case someNumber of
          IntegerNumber i
            | ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                                        , argument = ConstantType (TypeConstant {name = NatTypeName nat})
                                        } <- typ ->
              DecimalNumber (decimalFromInteger i nat)
          DecimalNumber d@Decimal {places}
            | ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                                        , argument = ConstantType (TypeConstant {name = NatTypeName nat})
                                        } <- typ
            , nat /= places -> DecimalNumber (expandDecimalPrecision nat d)
          _ -> someNumber
    }

--------------------------------------------------------------------------------
-- Get NF type from general type

toT :: Type Parsed -> Maybe T
toT =
  \case
    ConstantType TypeConstant {name = IntegerTypeName} -> pure IntegerT
    ConstantType TypeConstant {name = TextTypeName} -> pure TextT
    ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                              , argument = ConstantType (TypeConstant {name = NatTypeName nat})
                              } -> pure (DecimalT nat)
    ArrayType t -> do
      a <- toT t
      pure (ArrayT (pure a))
    -- TODO: Examine whether to preserve the row variable for variants.
    VariantType (RowType (TypeRow {typeVariable = _, fields = fs})) -> do
      fs' <-
        traverse
          (\Field{typ, name = FieldName name} -> do
             t' <- toT typ
             pure (TagName name, t'))
          fs
      pure (VariantT (OM.fromList fs'))
    RecordType (RowType (TypeRow {typeVariable = Nothing, fields = fs})) -> do
      fs' <-
        traverse
          (\Field{typ, name} -> do
             t' <- toT typ
             pure (name, t'))
          fs
      pure (RecordT (OM.fromList fs'))
    _ -> Nothing
