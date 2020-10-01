{-# LANGUAGE FlexibleContexts #-}
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
  , constraintsTypeVariablesGeneralised
  , constraintsTypeVariablesPolymorphic
  ) where

import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Inflex.Types
import           Inflex.Types.Defaulter ()
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
  -> Identity (Maybe (Type Polymorphic))
suggestTypeConstant =
  fmap (listToMaybe . map snd . sortBy (flip (comparing fst)) . catMaybes) .
  traverse suggestedConstant . toList
  where
    suggestedConstant ::
         ClassConstraint s
      -> Identity (Maybe (Natural, Type Polymorphic))
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

-- | Get type variables and each constraint that they're mentioned in,
-- for a generalised type.
constraintsTypeVariablesGeneralised ::
     Foldable t
  => t (ClassConstraint Generalised)
  -> Map (TypeVariable Generalised) (Set (ClassConstraint Generalised))
constraintsTypeVariablesGeneralised = constraintsTypeVariablesGeneric

-- | Get type variables and each constraint that they're mentioned in,
-- for a polymorphic type.
constraintsTypeVariablesPolymorphic ::
     Foldable t
  => t (ClassConstraint Polymorphic)
  -> Map (TypeVariable Polymorphic) (Set (ClassConstraint Polymorphic))
constraintsTypeVariablesPolymorphic = constraintsTypeVariablesGeneric

-- | Obtain the type variables mentioned in class constraints.
--
-- Example:
--
-- f(C a => C b => a -> b -> c) => {a,b}
constraintsTypeVariablesGeneric ::
     forall s t. (Foldable t, Ord (TypeVariable s), Ord (ClassConstraint s))
  => t (ClassConstraint s)
  -> Map (TypeVariable s) (Set (ClassConstraint s))
constraintsTypeVariablesGeneric constraints =
  M.fromListWith
    (<>)
    (concatMap
       (\classConstraint@ClassConstraint {typ = types} ->
          [ (typeVariable, Set.singleton classConstraint)
          | typeVariable <- toList (foldMap typeVariables types)
          ])
       constraints)
  where
    typeVariables :: Type s -> Set (TypeVariable s)
    typeVariables =
      \case
        RecordType t -> typeVariables t
        -- For s=Polymorphic, this is not possible. For s=Generalised,
        -- this is not relevant. In both cases we only want
        -- VariableType, below.
        PolyType {} -> mempty
        VariableType typeVariable -> Set.singleton typeVariable
        ApplyType TypeApplication {function, argument} ->
          typeVariables function <> typeVariables argument
        ConstantType {} -> mempty
        RowType TypeRow {typeVariable = _, fields}
                                   -- maybe mempty Set.singleton typeVariable <> -- TODO: Check this is fine.
         -> foldMap (\Field {typ} -> typeVariables typ) fields
