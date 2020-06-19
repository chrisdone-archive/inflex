{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Resolve class constraints into dictionaries.

module Inflex.Resolver where

import           Control.Monad.State
import           Control.Monad.Validate
import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Inflex.Generaliser
import           Inflex.Location
import           Inflex.Types
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Types

-- Result of resolving.
--
-- 1. An instance was found and inserted inline.
-- 2. No instance was found with polytypes.
-- 3. No instance can be found for constant type.
data ResolutionSuccess
  = InstanceFound InstanceName
  | NoInstanceButPoly (TypeVariable Polymorphic)
  deriving (Show, Eq)

-- 1. The user put 2.52 when accuracy was 0.0.
-- 2. Unsupported instance head.
-- 3. No instance was found with monotypes.
-- 4. Invalid type for class instance heads.
data ResolutionError
  = LiteralDecimalPrecisionMismatch PrecisionMismatch
  | UnsupportedInstanceHead
  | NoInstanceAndMono (TypeVariable Generalised)
  | NoInstanceForConstantType (TypeConstant Generalised)
  deriving (Show, Eq)

data PrecisionMismatch = PrecisionMismatch
  { placesAsWritten :: !Natural
  , placesAvailable :: !Natural
  , constraint :: !(ClassConstraint Generalised)
  } deriving (Show, Eq)

data GeneraliseResolveError
  = ResolverErrors (NonEmpty ResolutionError)
  | GeneraliserErrored SolveGeneraliseError
  deriving (Show, Eq)

data IsResolved a = IsResolved
  { thing :: !a
  , scheme :: !(Scheme Polymorphic)
  , mappings :: !(Map Cursor SourceLocation)
  , implicits :: !(Seq Implicit)
  } deriving (Show, Eq)

data ResolveState = ResolveState
  { implicits :: !(Seq Implicit)
  }

newtype Resolve a = Resolve
  { runResolve  :: ValidateT (NonEmpty ResolutionError) (State ResolveState) a
  } deriving (Functor, Applicative, Monad, MonadState ResolveState)

--------------------------------------------------------------------------------
-- Top-level

resolveText ::
     FilePath
  -> Text
  -> Either GeneraliseResolveError (IsResolved (Expression Resolved))
resolveText fp text = do
  IsGeneralised {thing, polytype, mappings} <-
    first GeneraliserErrored (generaliseText fp text)
  expression <-
    first
      ResolverErrors
      (evalState
         (runValidateT (runResolve (expressionResolver thing)))
         ResolveState {implicits = mempty})
  pure
    IsResolved
      { mappings
      , thing = expression
      , scheme =
          Scheme
            { location = expressionLocation thing
            , constraints = [] -- TODO: Collect constraints from state monad.
            , typ = polytype
            }
      , implicits = mempty -- TODO: Collect implicits from state monad, and apply lambdas.
      }

--------------------------------------------------------------------------------
-- Resolving expression tree

expressionResolver :: Expression Generalised -> Resolve (Expression Resolved)
expressionResolver = undefined

--------------------------------------------------------------------------------
-- Instance resolution

-- | Resolve a class constraint.
--
-- Currently, there is no instances list. We have no user-definable
-- instances or classes. Therefore it's a trivial piece of logic to check that:
--
-- * An Integer type matches with FromDecimal or FromInteger.
-- * A Decimal i type matches any FromDecimal j provided i<=j.
--
resolveConstraint ::
     ClassConstraint Generalised -> Either ResolutionError ResolutionSuccess
resolveConstraint constraint@ClassConstraint {className, typ} =
  undefined
  {-case typ of
    ApplyType {} -> Left UnsupportedInstanceHead
    VariableType typeVariable -> Left (NoInstanceAndMono typeVariable)
    PolyType typeVariable -> pure (NoInstanceButPoly typeVariable)
    ConstantType typeConstant@TypeConstant {name} ->
      case className of
        FromIntegerClassName ->
          case name of
            IntegerTypeName -> pure (InstanceFound FromIntegerIntegerInstance)
            DecimalTypeName {-places-} -> undefined
              -- pure (InstanceFound (FromIntegerDecimalInstance places))
            _ -> Left (NoInstanceForConstantType typeConstant)
        FromDecimalClassName {-placesAsWritten-} ->
          case name of
            DecimalTypeName {-placesAvailable-} ->
              undefined
              {-if placesAsWritten <= placesAvailable
                then pure
                       (InstanceFound
                          (FromDecimalDecimalInstance
                             FromDecimalInstance
                               { supersetPlaces = placesAvailable
                               , subsetPlaces = placesAsWritten
                               }))
                else Left
                       (LiteralDecimalPrecisionMismatch
                          PrecisionMismatch
                            {placesAsWritten, placesAvailable, constraint})-}
            _ -> Left (NoInstanceForConstantType typeConstant)-}
