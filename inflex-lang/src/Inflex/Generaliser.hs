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

data SolveGeneraliseError
  = GeneraliserErrors (NonEmpty GeneraliseError)
  | SolverErrored GenerateSolveError
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
     FilePath
  -> Text
  -> Either SolveGeneraliseError (IsGeneralised (Expression Generalised))
generaliseText fp text = do
  IsSolved {thing, mappings} <- first SolverErrored (solveText fp text)
  let (polytype, substitions) = toPolymorphic (expressionType thing)
  pure
    IsGeneralised
      { mappings
      , thing = expressionGeneralise substitions thing
      , polytype
      }

--------------------------------------------------------------------------------
-- Polymorphise a type

toPolymorphic :: Type Solved -> (Type Polymorphic, Map (TypeVariable Solved) (TypeVariable Polymorphic))
toPolymorphic =
  second replacements .
  flip runState GeneraliseState {counter = 0, replacements = mempty} . go
  where
    go =
      \case
        VariableType typeVariable@TypeVariable {kind} -> do
          replacements <- gets replacements
          case M.lookup typeVariable replacements of
            Nothing -> do
              index <- gets counter
              let typeVariable' =
                    TypeVariable {index, prefix = (), location = (), kind}
              put
                (GeneraliseState
                   { counter = index + 1
                   , replacements =
                       M.insert typeVariable typeVariable' replacements
                   })
              pure (VariableType typeVariable')
            Just replacement -> pure (VariableType replacement)
        ApplyType TypeApplication {function, argument, location, kind} -> do
          function' <- go function
          argument' <- go argument
          pure
            (ApplyType
               TypeApplication
                 {function = function', argument = argument', location, kind})
        ConstantType TypeConstant {..} ->
          pure (ConstantType TypeConstant {..})

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
        VariableType typeVariable@TypeVariable {..} ->
          case M.lookup typeVariable substitutions of
            Nothing -> VariableType TypeVariable {..}
            Just replacement -> PolyType replacement
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        ConstantType TypeConstant {..} -> ConstantType TypeConstant {..}

expressionGeneralise ::
     Map (TypeVariable Solved) (TypeVariable Polymorphic)
  -> Expression Solved
  -> Expression Generalised
expressionGeneralise substitutions =
  \case
    LiteralExpression literal ->
      LiteralExpression (literalGeneralise substitutions literal)
    LambdaExpression lambda ->
      LambdaExpression (lambdaGeneralise substitutions lambda)
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
        FromIntegerGlobal -> FromIntegerGlobal
        FromDecimalGlobal -> FromDecimalGlobal

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
