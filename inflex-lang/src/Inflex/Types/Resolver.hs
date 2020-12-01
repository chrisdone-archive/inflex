{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Types.Resolver where

import           Control.Monad.State
import           Control.Monad.Validate
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq(..))
import           Inflex.Generaliser
import           Inflex.Types
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Types

-- Result of resolving.
--
-- 1. An instance was found and inserted inline.
-- 2. No instance was found with polytypes.
data ResolutionSuccess
  = InstanceFound InstanceName
  | NoInstanceButPoly (ClassConstraint Polymorphic)
  deriving (Show, Eq)

-- 1. The user put 2.52 when accuracy was 0.0.
-- 2. Unsupported instance head.
-- 3. No instance was found with monotypes.
-- 4. Invalid type for class instance heads.
data ResolutionError
  = LiteralDecimalPrecisionMismatch PrecisionMismatch
  | UnsupportedInstanceHead (ClassConstraint Polymorphic)
  | NoInstanceAndMono ClassName (TypeVariable Generalised)
  | NoInstanceForType ClassName (Type Polymorphic)
  deriving (Show, Eq)

-- An implicit argument.
data ImplicitArgument
  = ExactInstance InstanceName
  | DeferredDeBrujin DeBrujinOffset
  deriving (Show, Eq)

newtype DeBrujinOffset = DeBrujinOffset
  { unDeBrujinOffset :: Int
  } deriving (Show, Eq, Ord)

data PrecisionMismatch = PrecisionMismatch
  { supersetPlaces :: !Natural
  , subsetPlaces :: !Natural
  , constraint :: !(ClassConstraint Polymorphic)
  } deriving (Show, Eq)

data GeneraliseResolveError e
  = ResolverErrors (NonEmpty ResolutionError)
  | GeneraliserErrored (SolveGeneraliseError e)
  deriving (Show, Eq)

data IsResolved a = IsResolved
  { thing :: !a
  , scheme :: !(Scheme Polymorphic)
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Eq)

data ResolveState = ResolveState
  { implicits :: !(Seq (ClassConstraint Polymorphic))
    -- ^ Each implicit constraint is added to the end of the sequence.

    -- The de Brujin index is calculated as current_deBurjin_index + 1
    -- + offset.  Where offset = index (zero-based, left to right)
    -- from this implicits list.

    -- The same implicit argument may be used more than once, so a
    -- lookup is performed first.
    --
    -- Finally, these are added -- in the same order! -- as class
    -- constraints to the top-level scheme class constraints.
    --
    -- Nesting is
    --
    -- f c1 c2
    -- is
    -- \-> \_ -> .. f (idx+1+0) (idx+1+1) ..
    --     ^-------|        c1         c2
    -- ^-------------|
    -- has REVERSED order!
    -- :: C2, C1 => ..
  , defaulteds :: !(Map (TypeVariable Generalised) (Type Polymorphic))
  }

newtype Resolve a = Resolve
  { runResolve  :: ValidateT (NonEmpty ResolutionError) (State ResolveState) a
  } deriving (Functor, Applicative, Monad, MonadState ResolveState)
