{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Suggest types for the defaulter.

module Inflex.Defaulter.Suggest
  ( suggestTypeConstant
  ) where

import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Ord
import           Inflex.Types
import           Inflex.Types.Defaulter
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Infer an appropriate defaulted type for a set of constraints

-- | Given a set of constraints that are for a SINGLE type variable
-- (and @FromDecimal 2 n@ counts, or @FromInteger n@), produce an
-- appropriate constant type, for each, if possible. So we will have a
-- set of type constants. At the end, choose the most appropriate type
-- based on priority (see below).
--
-- It's not the responsibility of this function to determine validity
-- of instances. Just to produce a type @Integer@ or @Decimal n@.
--
-- Order of priority: FromDecimal x > FromDecimal y > FromInteger,
-- such that x > y.
suggestTypeConstant ::
     forall s. (StagedLocation s ~ Cursor)
  => NonEmpty (ClassConstraint s)
     -- ^ All of them must only refer to THE SAME, SINGLE type
     -- variable.
  -> Either DefaulterError (Maybe (Type Polymorphic))
suggestTypeConstant =
  fmap (listToMaybe . map snd . sortBy (flip (comparing fst)) . catMaybes) .
  traverse suggestedConstant . toList
  where
    suggestedConstant ::
         ClassConstraint s
      -> Either DefaulterError (Maybe (Natural, Type Polymorphic))
    suggestedConstant =
      \case
        ClassConstraint {className = FromIntegerClassName} ->
          pure
            (pure
               ( 0
               , ConstantType
                   TypeConstant
                     {location = DefaultedCursor, name = IntegerTypeName}))
        ClassConstraint {className = FromDecimalClassName, typ = params} ->
          case params of
            ConstantType TypeConstant {name = NatTypeName places, location} :| [_] ->
              pure
                (pure
                   ( places
                   , ApplyType
                       TypeApplication
                         { location = DefaultedCursor
                         , kind = TypeKind
                         , function =
                             ConstantType
                               TypeConstant
                                 { location = DefaultedCursor
                                 , name = DecimalTypeName
                                 }
                         , argument =
                             ConstantType
                               TypeConstant
                                 {location, name = NatTypeName places}
                         }))
            _ -> pure Nothing
        _ -> pure Nothing
