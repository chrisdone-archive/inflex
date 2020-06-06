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
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Inflex.Solver
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
  , mappings :: !(Map Cursor SourceLocation)
  , classes :: !(Seq (ClassConstraint Generalised))
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

generalizeText ::
     FilePath
  -> Text
  -> Either SolveGeneraliseError (IsGeneralised (Expression Generalised))
generalizeText fp text = do
  IsSolved {thing, mappings, classes} <- first SolverErrored (solveText fp text)
  undefined

--------------------------------------------------------------------------------
-- Produce a polymorphic type

toPolymorphic :: Type Solved -> (Type Polymorphic, Map (TypeVariable Solved) (TypeVariable Polymorphic))
toPolymorphic =
  second replacements .
  flip runState GeneraliseState {counter = 0, replacements = mempty} . go
  where
    go =
      \case
        VariableType typeVariable@TypeVariable {location, kind} -> do
          replacements <- gets replacements
          case M.lookup typeVariable replacements of
            Nothing -> do
              index <- gets counter
              let typeVariable' =
                    TypeVariable {index, prefix = (), location, kind}
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
        ConstantType TypeConstant {..} -> pure (ConstantType TypeConstant {..})
