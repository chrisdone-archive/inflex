{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Generalise monomorphic types to poly types.

module Inflex.Generaliser where

import           Control.Monad.State
import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Void
import           Inflex.Solver
import           Inflex.Type
import           Inflex.Types
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Generalizer types

data GeneraliseError
  = OccursCheckFail (TypeVariable Generated) (Type Generated)
  | KindMismatch (TypeVariable Generated) (Type Generated)
  | TypeMismatch EqualityConstraint
  deriving (Show, Eq)

data SolveGeneraliseError e
  = GeneraliserErrors (NonEmpty GeneraliseError)
  | SolverErrored (GenerateSolveError e)
  deriving (Show, Eq)

data IsGeneralised a = IsGeneralised
  { thing :: !a
  , polytype :: !(Type Polymorphic)
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Eq)

data Substitution = Substitution
  { before :: !(TypeVariable Generated)
  , after :: !(Type Generated)
  } deriving (Show, Eq)

data GeneraliseState = GeneraliseState
  { counter :: !Natural
  , replacements :: !(Map (TypeVariable Solved) (TypeVariable Polymorphic))
  }

--------------------------------------------------------------------------------
-- Top-level

generaliseText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (SolveGeneraliseError e) (IsGeneralised (Expression Generalised))
generaliseText globals fp text = do
  solved <- first SolverErrored (solveText globals fp text)
  generaliseSolved solved

generaliseSolved ::
     IsSolved (Expression Solved)
  -> Either (SolveGeneraliseError e)  (IsGeneralised (Expression Generalised))
generaliseSolved IsSolved {thing, mappings} = do
  let (polytype, substitions) = toPolymorphic (expressionType thing)
  pure
    IsGeneralised
      {mappings, thing = expressionGeneralise substitions thing, polytype}

--------------------------------------------------------------------------------
-- Polymorphise a type

toPolymorphic :: Type Solved -> (Type Polymorphic, Map (TypeVariable Solved) (TypeVariable Polymorphic))
toPolymorphic =
  second replacements .
  flip runState GeneraliseState {counter = 0, replacements = mempty} . go
  where
    go =
      \case
        FreshType v -> absurd v
        RecordType t -> fmap RecordType (go t)
        VariantType t -> fmap VariantType (go t)
        ArrayType t -> fmap ArrayType (go t)
        RowType TypeRow {..} -> do
          fields' <- traverse rewriteField fields
          typeVariable' <- traverse polymorphizeTypeVar typeVariable
          pure
            (RowType
               TypeRow {fields = fields', typeVariable = typeVariable', ..})
        VariableType typeVariable ->
          fmap VariableType (polymorphizeTypeVar typeVariable)
        ApplyType TypeApplication {function, argument, location, kind} -> do
          function' <- go function
          argument' <- go argument
          pure
            (ApplyType
               TypeApplication
                 {function = function', argument = argument', location, kind})
        ConstantType TypeConstant {..} -> pure (ConstantType TypeConstant {..})
    rewriteField Field {..} = do
      typ' <- go typ
      pure Field {typ = typ', ..}
    polymorphizeTypeVar typeVariable@TypeVariable {kind} = do
      replacements <- gets replacements
      case M.lookup typeVariable replacements of
        Nothing -> do
          index <- gets counter
          let typeVariable' =
                TypeVariable {index, prefix = (), location = (), kind}
          put
            (GeneraliseState
               { counter = index + 1
               , replacements = M.insert typeVariable typeVariable' replacements
               })
          pure (typeVariable')
        Just replacement -> pure (replacement)

--------------------------------------------------------------------------------
-- Generalising (i.e. substitution, but we also change the type from
-- Solved to Generalised)

generaliseType ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Type Solved
  -> Type Generalised
generaliseType substitutions = go
  where
    go =
      \case
        FreshType v -> absurd v
        RecordType t -> RecordType (go t)
        VariantType t -> VariantType (go t)
        ArrayType t -> ArrayType (go t)
        VariableType typeVariable@TypeVariable {..} ->
          case M.lookup typeVariable substitutions of
            Nothing -> VariableType TypeVariable {..}
            Just replacement -> PolyType replacement
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        ConstantType TypeConstant {..} -> ConstantType TypeConstant {..}
        RowType TypeRow {..} ->
          RowType
            TypeRow
              { fields = fmap fieldSolve fields
              , typeVariable = fmap typeVarSolve typeVariable
              , ..
              }
    fieldSolve Field {..} = Field {typ = generaliseType substitutions typ, ..}
    typeVarSolve TypeVariable {..} = TypeVariable {..}

expressionGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Expression Solved
  -> Expression Generalised
expressionGeneralise substitutions =
  \case
    LiteralExpression literal ->
      LiteralExpression (literalGeneralise substitutions literal)
    PropExpression prop ->
      PropExpression (propGeneralise substitutions prop)
    HoleExpression hole ->
      HoleExpression (holeGeneralise substitutions hole)
    ArrayExpression array ->
      ArrayExpression (arrayGeneralise substitutions array)
    VariantExpression variant ->
      VariantExpression (variantGeneralise substitutions variant)
    RecordExpression record ->
      RecordExpression (recordGeneralise substitutions record)
    LambdaExpression lambda ->
      LambdaExpression (lambdaGeneralise substitutions lambda)
    LetExpression let' ->
      LetExpression (letGeneralise substitutions let')
    InfixExpression infix' ->
      InfixExpression (infixGeneralise substitutions infix')
    ApplyExpression apply ->
      ApplyExpression (applyGeneralise substitutions apply)
    VariableExpression variable ->
      VariableExpression (variableGeneralise substitutions variable)
    GlobalExpression global ->
      GlobalExpression (globalGeneralise substitutions global)

globalGeneralise :: Map (TypeVariable Solved) (TypeVariable Polymorphic) -> Global Solved -> Global Generalised
globalGeneralise substitutions Global {scheme = SolvedScheme scheme, ..} =
  Global
    { scheme = GeneralisedScheme (generaliseScheme substitutions scheme)
    , name = refl
    , ..
    }
  where
    refl =
      case name of
        FunctionGlobal f -> FunctionGlobal f
        FromIntegerGlobal -> FromIntegerGlobal
        EqualGlobal -> EqualGlobal
        FromDecimalGlobal -> FromDecimalGlobal
        NumericBinOpGlobal n -> NumericBinOpGlobal n
        HashGlobal x -> HashGlobal x

generaliseScheme :: Map (TypeVariable Solved) (TypeVariable Polymorphic) -> Scheme Solved -> Scheme Generalised
generaliseScheme substitutions Scheme {..} =
  Scheme
    { typ = generaliseType substitutions typ
    , constraints = fmap (generaliseClassConstraint substitutions) constraints
    , ..
    }

generaliseClassConstraint ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> ClassConstraint Solved
  -> ClassConstraint Generalised
generaliseClassConstraint substitutions ClassConstraint {..} =
  ClassConstraint {typ = fmap (generaliseType substitutions) typ, ..}

recordGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Record Solved
  -> Record Generalised
recordGeneralise substitutions Record {..} =
  Record
    { fields =
        map
          (\FieldE {location = l, ..} ->
             FieldE
               { expression = expressionGeneralise substitutions expression
               , location = l
               , ..
               })
          fields
    , typ = generaliseType substitutions typ
    , ..
    }

propGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Prop Solved
  -> Prop Generalised
propGeneralise substitutions Prop {..} =
  Prop
    { expression = expressionGeneralise substitutions expression
    , typ = generaliseType substitutions typ
    , ..
    }

holeGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Hole Solved
  -> Hole Generalised
holeGeneralise substitutions Hole {..} =
  Hole
    { typ = generaliseType substitutions typ
    , ..
    }

arrayGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Array Solved
  -> Array Generalised
arrayGeneralise substitutions Array {..} =
  Array
    { expressions = fmap (expressionGeneralise substitutions) expressions
    , typ = generaliseType substitutions typ
    , ..
    }

variantGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Variant Solved
  -> Variant Generalised
variantGeneralise substitutions Variant {..} =
  Variant
    { argument = fmap (expressionGeneralise substitutions) argument
    , typ = generaliseType substitutions typ
    , ..
    }

lambdaGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Lambda Solved
  -> Lambda Generalised
lambdaGeneralise substitutions Lambda {..} =
  Lambda
    { param = paramGeneralise substitutions param
    , body = expressionGeneralise substitutions body
    , typ = generaliseType substitutions typ
    , ..
    }

letGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Let Solved
  -> Let Generalised
letGeneralise substitutions Let {..} =
  Let
    { binds = fmap (bindGeneralise substitutions) binds
    , body = expressionGeneralise substitutions body
    , typ = generaliseType substitutions typ
    , ..
    }

infixGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Infix Solved
  -> Infix Generalised
infixGeneralise substitutions Infix {..} =
  Infix
    { left = expressionGeneralise substitutions left
    , right = expressionGeneralise substitutions right
    , global = globalGeneralise substitutions global
    , typ = generaliseType substitutions typ
    , ..
    }

bindGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Bind Solved
  -> Bind Generalised
bindGeneralise substitutions Bind {..} =
  Bind
    { param = paramGeneralise substitutions param
    , value = expressionGeneralise substitutions value
    , typ = generaliseType substitutions typ
    , ..
    }

applyGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Apply Solved
  -> Apply Generalised
applyGeneralise substitutions Apply {..} =
  Apply
    { function = expressionGeneralise substitutions function
    , argument = expressionGeneralise substitutions argument
    , typ = generaliseType substitutions typ
    , ..
    }

variableGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Variable Solved
  -> Variable Generalised
variableGeneralise substitutions Variable {..} =
  Variable {typ = generaliseType substitutions typ, ..}

literalGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Literal Solved
  -> Literal Generalised
literalGeneralise substitutions =
  \case
    TextLiteral LiteralText {..} ->
      TextLiteral LiteralText {typ = generaliseType substitutions typ, ..}
    NumberLiteral number ->
      NumberLiteral (numberGeneralise substitutions number)

numberGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Number Solved
  -> Number Generalised
numberGeneralise substitutions Number {..} =
  Number {typ = generaliseType substitutions typ, ..}

paramGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Param Solved
  -> Param Generalised
paramGeneralise substitutions Param {..} =
  Param {typ = generaliseType substitutions typ, ..}
