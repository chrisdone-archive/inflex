{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
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

-- | Solve equality constraints.

module Inflex.Types.Solver
  ( Substitution(..)
  , SolveError(..)
  , IsSolved(..)
  , GenerateSolveError(..)
  , SolveReader(..)
  , SolveMsg(..)
  , Solve(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.HashMap.Strict (HashMap)

import           Data.Map.Strict (Map)
import           Inflex.Generator
import           Inflex.Optics
import           Inflex.Types
import           Numeric.Natural
import           Optics.Lens
import           Optics.TH
import           RIO (HasStateRef(..), RIO, GLogFunc, HasGLogFunc(..), SomeRef)

data SolveError
  = OccursCheckFail (TypeVariable Generated) (Type Generated)
  | KindMismatch (TypeVariable Generated) (Type Generated)
  | TypeMismatch EqualityConstraint
  | RowMismatch (TypeRow Generated) (TypeRow Generated)
  | NotRowTypes
  deriving (Show, Eq)

data GenerateSolveError e
  = SolverError SolveError
  | GeneratorErrored (RenameGenerateError e)
  deriving (Show, Eq)

data IsSolved a = IsSolved
  { thing :: !a
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Eq)

data Substitution = Substitution
  { before :: !(TypeVariable Generated)
  , after :: !(Type Generated)
  } deriving (Show, Eq)

data SolveReader = SolveReader
  { glogfunc :: GLogFunc SolveMsg
  , counter :: SomeRef Natural
  , binds :: SomeRef (HashMap (TypeVariable Generated) (SomeRef (Type Generated)))
  }

data SolveMsg
  = UnifyConstraints Int
  | UnifyConstraintsComplete Int
  | UnifyConstraintsIterate Int
  | UnifyEqualityConstraint EqualityConstraint
  | UnifyTypeApplications
  | UnifyRows (TypeRow Generated) (TypeRow Generated)
  | SuccessfulBindTypeVariable Substitution
  | GeneratedTypeVariable TypeVariablePrefix Kind Natural
  deriving (Show)

$(makeLensesWith (inflexRules ['glogfunc, 'counter]) ''SolveReader)

instance HasGLogFunc SolveReader where
  type GMsg SolveReader = SolveMsg
  gLogFuncL = toLensVL solveReaderGlogfuncL

instance HasStateRef Natural SolveReader where
  stateRefL = toLensVL solveReaderCounterL

newtype Solve a = Solve
  { runSolve :: RIO SolveReader a
  } deriving ( MonadState Natural
             , Monad
             , Functor
             , Applicative
             , MonadIO
             , MonadReader SolveReader
             )
