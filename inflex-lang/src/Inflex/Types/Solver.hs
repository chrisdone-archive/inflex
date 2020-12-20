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

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Inflex.Generator
import Inflex.Optics
import Inflex.Types
import Numeric.Natural
import Optics.Lens
import Optics.TH
import RIO (RIO, GLogFunc, HasGLogFunc(..))

data SolveError
  = OccursCheckFail (TypeVariable Generated) (Type Generated)
  | KindMismatch (TypeVariable Generated) (Type Generated)
  | TypeMismatch EqualityConstraint
  | RowMismatch (TypeRow Generated) (TypeRow Generated)
  deriving (Show, Eq)

data GenerateSolveError e
  = SolverErrors (NonEmpty SolveError)
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

newtype Solve a = Solve
  { runSolve :: StateT Natural (ExceptT (NonEmpty SolveError) (RIO SolveReader)) a
  } deriving ( MonadState Natural
             , Monad
             , Functor
             , Applicative
             , MonadError (NonEmpty SolveError)
             , MonadIO
             , MonadReader SolveReader
             )

data SolveReader =
  SolveReader {glogfunc :: GLogFunc SolveMsg}

data SolveMsg = UnifyConstraints

$(makeLensesWith (inflexRules ['glogfunc]) ''SolveReader)

instance HasGLogFunc SolveReader where
  type GMsg SolveReader = SolveMsg
  gLogFuncL = toLensVL solveReaderGlogfuncL
