{-# LANGUAGE DeriveGeneric #-}
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
  ) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OM
import           Data.Text (Text)
import           GHC.Generics
import           GHC.Natural
import           Inflex.Generator
import           Inflex.Parser2
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
  deriving (Show, Eq, Generic)

data T
  = ArrayT !(Maybe T)
  | RecordT !(InsOrdHashMap FieldName T)
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
  case expressionType expression >>= toT of
    Nothing -> Left NoTypeSig
    Just sigT -> do
      inferredT <- expressionGenerate expression
      finalT <- unifyT sigT inferredT
      apply expression (toTypeMono finalT)

--------------------------------------------------------------------------------
-- Generation

expressionGenerate :: Expression Parsed -> Either NormalFormCheckProblem T
expressionGenerate =
  \case
    LiteralExpression literal -> pure $! (literalGenerator literal)
    ArrayExpression array -> arrayGenerate array
    RecordExpression record -> fmap RecordT (recordGenerate record)
    VariantExpression {} -> Left NotNormalForm -- TODO:
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
  if OM.keys x == OM.keys y
    then do
      !m <-
        fmap
          OM.fromList
          (traverse
             (\((k1, v1), v2) -> do
                t <- unifyT v1 v2
                pure (k1, t))
             (zip (OM.toList x) (OM.elems y)))
      pure (RecordT m)
    else Left (RecordFieldsMismatch (OM.keys x) (OM.keys y))
-- Promotion of integer to decimal:
unifyT IntegerT (DecimalT n) = pure (DecimalT n)
unifyT (DecimalT n) IntegerT = pure (DecimalT n)
-- Promotion of smaller decimal to larger decimal:
unifyT (DecimalT x) (DecimalT y) = pure (DecimalT n)
  where !n = max x y
unifyT x y = Left (TypeMismatch x y)

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
          NumberLiteral number -> NumberLiteral number {typ, location = BuiltIn}
          TextLiteral text -> TextLiteral text {typ, location = BuiltIn}))
apply (ArrayExpression array@Array {expressions}) (ArrayType typ) = do
  expressions' <- traverse (flip apply typ) expressions
  pure
    (ArrayExpression
       array
         {expressions = expressions', location = BuiltIn, typ = ArrayType typ})
apply RecordExpression {} _ = error "TODO"
apply _ _ = Left NotNormalForm

--------------------------------------------------------------------------------
-- Get NF type from general type

toT :: Type Parsed -> Maybe T
toT =
  \case
    ConstantType TypeConstant {name = IntegerTypeName} -> pure IntegerT
    ArrayType t -> do
      a <- toT t
      pure (ArrayT (pure a))
    -- TODO: DecimalT
    _ -> Nothing
