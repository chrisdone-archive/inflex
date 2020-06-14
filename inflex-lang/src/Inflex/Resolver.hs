{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Resolve class constraints into dictionaries.

module Inflex.Resolver where

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Inflex.Generaliser
import           Inflex.Types

--------------------------------------------------------------------------------
-- Top-level

data ResolveError
  = OccursCheckFail (TypeVariable Generated) (Type Generated)
  | KindMismatch (TypeVariable Generated) (Type Generated)
  | TypeMismatch EqualityConstraint
  deriving (Show, Eq)

data GeneraliseResolveError
  = ResolverErrors (NonEmpty ResolveError)
  | GeneraliserErrored SolveGeneraliseError
  deriving (Show, Eq)

data IsResolved a = IsResolved
  { thing :: !a
  , scheme :: !(Scheme Polymorphic)
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level

resolveText ::
     FilePath
  -> Text
  -> Either GeneraliseResolveError (IsResolved (Expression Resolved))
resolveText fp text = do
  IsGeneralised {thing, polytype, mappings} <-
    first GeneraliserErrored (generaliseText fp text)
  pure IsResolved {mappings, thing = undefined, scheme = undefined}

--------------------------------------------------------------------------------
-- Resolving numbers

-- | Try to resolve the instance for the given number. May result in one of:
--
-- 1. An instance was found and inserted inline.
-- 2. No instance was found with polytypes, so we defer and add an
--    argument and constraint to the declaration.
-- 3. No instance was found with monotypes, so we try to default.
-- 4. If we cannot default, an error is raised.
numberResolver :: Number Generalised -> Number Resolved
numberResolver Number {typ, number, location} =
  undefined

-- | Given a number, produce the same type with the right class
-- constraint wrapped around it.
--
-- 1 :: FromInteger i => i
-- 1 :: FromInteger Integer => Integer -- if inferred.
-- 2.3 :: FromDecimal 1 i => i -- 1 decimal place.
-- 7.00 :: FromDecimal 2 (Decimal 2) => Decimal 2 -- if inferred.
numberScheme :: Number Generalised -> Scheme Generalised
numberScheme Number {typ, number, location} =
  Scheme
    { constraints =
        pure (ClassConstraint {className = someNumberClassName number, ..})
    , ..
    }

-- | Tells us which class constraint arises from some number literal.
someNumberClassName :: SomeNumber -> ClassName
someNumberClassName =
  \case
    IntegerNumber {} -> FromIntegerClassName
    DecimalNumber Decimal {places} -> FromDecimalClassName places
