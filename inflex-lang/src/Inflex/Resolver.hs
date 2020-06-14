{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Resolve class constraints into dictionaries.

module Inflex.Resolver where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Inflex.Generaliser
import Inflex.Types

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
