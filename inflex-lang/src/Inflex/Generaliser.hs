{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Generalise monomorphic types to poly types.

module Inflex.Generaliser where

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Instances
import           Inflex.Solver
import           Inflex.Types

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

--------------------------------------------------------------------------------
-- Top-level

generalizeText ::
     FilePath
  -> Text
  -> Either SolveGeneraliseError (IsGeneralised (Expression Generalised))
generalizeText fp text = do
  IsSolved {thing, mappings, classes} <- first SolverErrored (solveText fp text)
  undefined
