{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Resolve class constraints into dictionaries.

module Inflex.Resolver where

import Control.Monad.State
import Control.Monad.Validate
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Inflex.Generaliser
import Inflex.Location
import Inflex.Types
import Numeric.Natural

--------------------------------------------------------------------------------
-- Types

-- Result of resolving.
--
-- 1. An instance was found and inserted inline.
-- 2. No instance was found with polytypes.
-- 3. No instance can be found for constant type.
data ResolutionSuccess
  = InstanceFound
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
  } deriving (Show, Eq)

data ResolveState =
  ResolveState

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
  case thing of
    LiteralExpression (NumberLiteral number) -> do
      number' <-
        first
          ResolverErrors
          (evalState
             (runValidateT (runResolve (numberResolver number)))
             ResolveState)
      pure
        IsResolved
          { mappings
          , thing = LiteralExpression (NumberLiteral number')
          , scheme =
              Scheme
                { location = expressionLocation thing
                , constraints = []
                , typ = polytype
                }
          }
    _ -> undefined

--------------------------------------------------------------------------------
-- Resolving numbers

-- | Try to resolve the instance for the given number. May result in one of:
--
-- 1. An instance was found and inserted inline.
-- 2. No instance was found with polytypes, so we defer and add an
--    argument and constraint to the declaration.
-- 3. No instance was found with monotypes, so we try to default.
-- 4. If we cannot default, an error is raised.
numberResolver :: Number Generalised -> Resolve (Number Resolved)
numberResolver number'@Number{..} =
  case resolveConstraint (numberConstraint number') of
    Right resolution -> pure Number {..}
    Left problem -> Resolve (refute (pure problem))

-- | Given a number, the appropriate class constraint.
--
-- 1 :: FromInteger i => i
-- 1 :: FromInteger Integer => Integer -- if inferred.
-- 2.3 :: FromDecimal 1 i => i -- 1 decimal place.
-- 7.00 :: FromDecimal 2 (Decimal 2) => Decimal 2 -- if inferred.
numberConstraint :: Number Generalised -> ClassConstraint Generalised
numberConstraint Number {typ, number, location} =
  ClassConstraint {className = someNumberClassName number, ..}

-- | Tells us which class constraint arises from some number literal.
someNumberClassName :: SomeNumber -> ClassName
someNumberClassName =
  \case
    IntegerNumber {} -> FromIntegerClassName
    DecimalNumber Decimal {places} -> FromDecimalClassName places

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
  case typ of
    ApplyType {} -> Left UnsupportedInstanceHead
    VariableType typeVariable -> Left (NoInstanceAndMono typeVariable)
    PolyType typeVariable -> pure (NoInstanceButPoly typeVariable)
    ConstantType typeConstant@TypeConstant {name} ->
      case className of
        FromIntegerClassName ->
          case name of
            IntegerTypeName -> pure InstanceFound
            DecimalTypeName _places -> pure InstanceFound
            _ -> Left (NoInstanceForConstantType typeConstant)
        FromDecimalClassName placesAsWritten ->
          case name of
            DecimalTypeName placesAvailable ->
              if placesAvailable >= placesAsWritten
                then pure InstanceFound
                else Left
                       (LiteralDecimalPrecisionMismatch
                          PrecisionMismatch
                            {placesAsWritten, placesAvailable, constraint})
            _ -> Left (NoInstanceForConstantType typeConstant)
