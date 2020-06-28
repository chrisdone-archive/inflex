{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Solve equality constraints, updating all type variables in the AST.

module Inflex.Solver where

import Control.Monad
import Data.Bifunctor
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Inflex.Generator
import Inflex.Kind
import Inflex.Types

--------------------------------------------------------------------------------
-- Solver types

data SolveError
  = OccursCheckFail (TypeVariable Generated) (Type Generated)
  | KindMismatch (TypeVariable Generated) (Type Generated)
  | TypeMismatch EqualityConstraint
  deriving (Show, Eq)

data GenerateSolveError
  = SolverErrors (NonEmpty SolveError)
  | GeneratorErrored RenameGenerateError
  deriving (Show, Eq)

data IsSolved a = IsSolved
  { thing :: !a
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Eq)

data Substitution = Substitution
  { before :: !(TypeVariable Generated)
  , after :: !(Type Generated)
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level

solveText ::
     FilePath
  -> Text
  -> Either GenerateSolveError (IsSolved (Expression Solved))
solveText fp text = do
  HasConstraints {thing = expression, mappings, equalities} <-
    first GeneratorErrored (generateText fp text)
  first
    SolverErrors
    (do substitutions <- unifyConstraints equalities
        pure
          IsSolved
            { thing = expressionSolve substitutions expression
            , mappings
            })

unifyAndSubstitute ::
     Seq EqualityConstraint
  -> Type Generated
  -> Either (NonEmpty SolveError) (Type Solved)
unifyAndSubstitute equalities typ = do
  substitutions <- unifyConstraints equalities
  pure (solveType substitutions typ)

--------------------------------------------------------------------------------
-- Unification

unifyConstraints ::
     Seq EqualityConstraint -> Either (NonEmpty SolveError) (Seq Substitution)
unifyConstraints =
  foldM
    (\existing equalityConstraint ->
       fmap
         (\new -> extendSubstitutions Extension {existing, new})
         (unifyEqualityConstraint
            (substituteEqualityConstraint existing equalityConstraint)))
    mempty

unifyEqualityConstraint :: EqualityConstraint -> Either (NonEmpty SolveError) (Seq Substitution)
unifyEqualityConstraint equalityConstraint@EqualityConstraint {type1, type2} =
  case (type1, type2) of
    (ApplyType typeApplication1, ApplyType typeApplication2) ->
      unifyTypeApplications typeApplication1 typeApplication2
    (VariableType typeVariable, typ) -> bindTypeVariable typeVariable typ
    (typ, VariableType typeVariable) -> bindTypeVariable typeVariable typ
    (ConstantType TypeConstant {name = typeConstant1}, ConstantType TypeConstant {name = typeConstant2})
      | typeConstant1 == typeConstant2 -> pure mempty
    _ -> Left (pure (TypeMismatch equalityConstraint))

unifyTypeApplications ::
     TypeApplication Generated
  -> TypeApplication Generated
  -> Either (NonEmpty SolveError) (Seq Substitution)
unifyTypeApplications typeApplication1 typeApplication2 = do
  existing <-
    unifyEqualityConstraint
      EqualityConstraint {type1 = function1, type2 = function2, location}
  new <-
    unifyEqualityConstraint
      (substituteEqualityConstraint
         existing
         (EqualityConstraint {type1 = argument1, type2 = argument2, location}))
  pure (extendSubstitutions Extension {existing, new})
  where
    TypeApplication {function = function1, argument = argument1, location}
     = typeApplication1
     -- TODO: set location properly. This will enable "provenance"
     -- <https://www.youtube.com/watch?v=rdVqQUOvxSU>
    TypeApplication {function = function2, argument = argument2} =
      typeApplication2

--------------------------------------------------------------------------------
-- Binding

bindTypeVariable :: TypeVariable Generated -> Type Generated -> Either (NonEmpty SolveError) (Seq Substitution)
bindTypeVariable typeVariable typ
  | typ == VariableType typeVariable = pure mempty
  | occursIn typeVariable typ = Left (pure (OccursCheckFail typeVariable typ))
  | typeVariableKind typeVariable /= typeKind typ = Left (pure (KindMismatch typeVariable typ))
  | otherwise = pure (pure Substitution {before = typeVariable, after = typ})

occursIn :: TypeVariable Generated -> Type Generated -> Bool
occursIn typeVariable =
  \case
    VariableType typeVariable' -> typeVariable == typeVariable'
    ApplyType TypeApplication {function, argument} ->
      occursIn typeVariable function || occursIn typeVariable argument
    ConstantType {} -> False

--------------------------------------------------------------------------------
-- Extension

data Extension = Extension
  { existing :: Seq Substitution
  , new :: Seq Substitution
  }

extendSubstitutions :: Extension -> Seq Substitution
extendSubstitutions Extension {new, existing} = existing' <> new
  where
    existing' =
      fmap
        (\Substitution {after, ..} ->
           Substitution {after = substituteType new after, ..})
        existing

--------------------------------------------------------------------------------
-- Substitution

substituteEqualityConstraint ::
     Seq Substitution -> EqualityConstraint -> EqualityConstraint
substituteEqualityConstraint substitutions equalityConstraint =
  EqualityConstraint
    { type1 = substituteType substitutions type1
    , type2 = substituteType substitutions type2
    , ..
    }
  where
    EqualityConstraint {type1, type2, ..} = equalityConstraint

substituteType :: Seq Substitution -> Type Generated -> Type Generated
substituteType substitutions = go
  where
    go =
      \case
        typ@ConstantType {} -> typ
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        typ@(VariableType typeVariable :: Type Generated) ->
          case find
                 (\Substitution {before} -> before == typeVariable)
                 substitutions of
            Just Substitution {after} -> after
            Nothing -> typ

--------------------------------------------------------------------------------
-- Solving (i.e. substitution, but we also change the type from
-- Generated to Solved)

solveType :: Seq Substitution -> Type Generated -> Type Solved
solveType substitutions = go . substituteType substitutions
  where
    go =
      \case
        VariableType TypeVariable {..} -> VariableType TypeVariable {..}
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        ConstantType TypeConstant {..} -> ConstantType TypeConstant {..}

expressionSolve :: Seq Substitution -> Expression Generated -> Expression Solved
expressionSolve substitutions =
  \case
    LiteralExpression literal ->
      LiteralExpression (literalSolve substitutions literal)
    LambdaExpression lambda ->
      LambdaExpression (lambdaSolve substitutions lambda)
    ApplyExpression apply ->
      ApplyExpression (applySolve substitutions apply)
    VariableExpression variable ->
      VariableExpression (variableSolve substitutions variable)
    GlobalExpression global ->
      GlobalExpression (globalSolve substitutions global)

lambdaSolve :: Seq Substitution -> Lambda Generated -> Lambda Solved
lambdaSolve substitutions Lambda {..} =
  Lambda
    { param = paramSolve substitutions param
    , body = expressionSolve substitutions body
    , typ = solveType substitutions typ
    , ..
    }

applySolve :: Seq Substitution -> Apply Generated -> Apply Solved
applySolve substitutions Apply {..} =
  Apply
    { function = expressionSolve substitutions function
    , argument = expressionSolve substitutions argument
    , typ = solveType substitutions typ
    , ..
    }

variableSolve :: Seq Substitution -> Variable Generated -> Variable Solved
variableSolve substitutions Variable {..} =
  Variable {typ = solveType substitutions typ, ..}

globalSolve :: Seq Substitution -> Global Generated -> Global Solved
globalSolve substitutions Global {scheme = GeneratedScheme scheme, ..} =
  Global
    {scheme = SolvedScheme (solveScheme substitutions scheme), name = refl, ..}
  where
    refl =
      case name of
        FromIntegerGlobal -> FromIntegerGlobal
        FromDecimalGlobal -> FromDecimalGlobal

solveScheme :: Seq Substitution -> Scheme Generated -> Scheme Solved
solveScheme substitutions Scheme {..} =
  Scheme
    { typ = solveType substitutions typ
    , constraints = fmap (solveClassConstraint substitutions) constraints
    , ..
    }

solveClassConstraint :: Seq Substitution -> ClassConstraint Generated -> ClassConstraint Solved
solveClassConstraint substitutions ClassConstraint {..} =
  ClassConstraint {typ = fmap (solveType substitutions) typ, ..}

literalSolve :: Seq Substitution -> Literal Generated -> Literal Solved
literalSolve substitutions =
  \case
    NumberLiteral number ->
      NumberLiteral (numberSolve substitutions number)

numberSolve :: Seq Substitution -> Number Generated -> Number Solved
numberSolve substitutions Number {..} =
  Number {typ = solveType substitutions typ, ..}

paramSolve :: Seq Substitution -> Param Generated -> Param Solved
paramSolve substitutions Param {..} =
  Param {typ = solveType substitutions typ, ..}
