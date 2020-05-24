{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
-- |

module Inflex.Solver where

import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Inflex.Generator
import           Inflex.Types

--------------------------------------------------------------------------------
-- Solver types

data SolveError = SolveError
  deriving (Show, Eq)

newtype Solver a = Solver
  { runSolver :: Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             )

data ParseSolveError
  = SolverErrors (NonEmpty SolveError)
  | GeneratorErrored RenameGenerateError
  deriving (Show, Eq)

type CursorBuilder = Cursor -> Cursor

data IsSolved a = IsSolved
  { thing :: !a
  , mappings :: !(Map Cursor SourceLocation)
  , classes :: !(Seq (ClassConstraint Generated))
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level

solveText ::
     FilePath
  -> Text
  -> Either ParseSolveError (IsSolved (Expression Solved))
solveText fp text = do
  HasConstraints {thing = expression, mappings, classes} <-
    first GeneratorErrored (generateText fp text)
  first
    SolverErrors
    (let thing = runIdentity (runSolver (expressionSolver expression))
      in pure (IsSolved {thing, mappings, classes}))

--------------------------------------------------------------------------------
-- Solver

expressionSolver :: Expression Generated -> Solver (Expression Solved)
expressionSolver = error "TODO"
