{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Defaulting class instances that are ambiguous.

module Inflex.Defaulter
  ( defaultText
  , DefaulterError(..)
  , ResolverDefaulterError(..)
  ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Writer
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Inflex.Resolver
import           Inflex.Types
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Types

data DefaulterError
  = ResolutionError ResolutionError
  | DefaultingNoInstanceFound (ClassConstraint Polymorphic)
  | DO
  deriving (Eq, Show)

data ResolverDefaulterError
  = DefaulterError DefaulterError
  | GeneraliseResolverError GeneraliseResolveError
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Top-level entry points

defaultText :: Map Hash (Scheme Polymorphic) -> FilePath -> Text -> Either ResolverDefaulterError Cell
defaultText globals fp text = do
  resolved <- first GeneraliseResolverError (resolveText globals fp text)
  first DefaulterError (defaultResolvedExpression resolved)

defaultResolvedExpression ::
     IsResolved (Expression Resolved) -> Either DefaulterError Cell
defaultResolvedExpression IsResolved {scheme = scheme0, thing = expression} = do
  classConstraintReplacements <- generateReplacements scheme0
  (scheme', defaults) <-
    runWriterT
      (foldM
         (\scheme@Scheme {constraints = acc, typ} classConstraint ->
            case M.lookup classConstraint classConstraintReplacements of
              Nothing -> pure scheme {constraints = acc ++ [classConstraint]}
              Just replacements -> do
                let substitutions = Seq.fromList (toList replacements)
                    classConstraint' =
                      substituteClassConstraint substitutions classConstraint
                    typ' = substituteType substitutions typ
                default' <-
                  lift (makeValidDefault classConstraint classConstraint')
                tell (pure default')
                pure (scheme {constraints = acc, typ = typ'}))
         scheme0 {constraints = mempty}
         originalConstraints)
  pure
    Cell
      { location
      , scheme = scheme'
      , defaultedClassConstraints = defaults
      , ambiguousClassConstraints = mempty -- TODO: Provide this info.
      , expression = applyDefaults originalConstraints defaults expression
      }
  where
    Scheme {constraints = originalConstraints, location} = scheme0

-- | Generate replacements for each class constraint that can be generated.
generateReplacements ::
     Scheme Polymorphic
  -> Either DefaulterError (Map (ClassConstraint Polymorphic) (Set Substitution))
generateReplacements scheme0 = do
  typeVariableReplacements <-
    M.traverseMaybeWithKey
      (\_key constraints' ->
         fmap (fmap (constraints', )) (suggestTypeConstant constraints'))
      constrainedDefaultableTypeVariables
  let classConstraintReplacements =
        M.fromListWith
          (<>)
          (concatMap
             (\(typeVariable, (constraints', typ)) ->
                map
                  (\constraint ->
                     ( constraint
                     , Set.singleton
                         (Substitution {before = typeVariable, after = typ})))
                  (toList constraints'))
             (M.toList typeVariableReplacements))
  pure classConstraintReplacements
  where
    constrainedDefaultableTypeVariables =
      M.mapMaybe
        (NE.nonEmpty . toList)
        (M.intersectionWith
           (<>)
           (constraintedTypeVariables scheme0)
           (M.fromList
              (map (, mempty) (toList (defaultableTypeVariables scheme0)))))

--------------------------------------------------------------------------------
-- Applying defaults

-- | Traverse down the expression for each class constraint, and if
-- there is a default for that class constraint, apply a dictionary
-- argument to the lambda. If there's no default for that class
-- constraint, we step down into the lambda and continue.
applyDefaults ::
     [ClassConstraint Polymorphic]
  -> Seq (Default Polymorphic)
  -> Expression Resolved
  -> Expression Resolved
applyDefaults _originalClassConstraints _defaults expression = expression

--------------------------------------------------------------------------------
-- Generating a default from a class constraint

-- Uses Inflex.Resolver.resolveConstraint to check that the suggested
-- types correctly produce an instance for the class constraint.
--
-- If it produces a ResolutionError, that's a hard fail. If no instance
-- is found, that's a hard fail.

-- | We check to see whether the defaulted class constraint is valid.
makeValidDefault ::
     ClassConstraint Polymorphic
  -> ClassConstraint Polymorphic
  -> Either DefaulterError (Default Polymorphic)
makeValidDefault classConstraintOriginal classConstraintDefaulted = do
  resolutionSuccess <-
    first ResolutionError (resolvePolyConstraint classConstraintDefaulted)
  case resolutionSuccess of
    InstanceFound instanceName ->
      pure
        Default
          {classConstraintOriginal, classConstraintDefaulted, instanceName}
    NoInstanceButPoly noInstanceConstraint ->
      Left (DefaultingNoInstanceFound noInstanceConstraint)

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
     NonEmpty (ClassConstraint Polymorphic)
     -- ^ All of them must only refer to THE SAME, SINGLE type
     -- variable.
  -> Either DefaulterError (Maybe (Type Polymorphic))
suggestTypeConstant =
  fmap (listToMaybe . map snd . sortBy (flip (comparing fst)) . catMaybes) .
  traverse suggestedConstant . toList
  where
    suggestedConstant ::
         ClassConstraint Polymorphic
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
            ConstantType argument@TypeConstant {name = NatTypeName places} :| [_] ->
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
                         , argument = ConstantType argument
                         }))
            _ -> pure Nothing
        _ -> pure Nothing

--------------------------------------------------------------------------------
-- Type variables mentioned in the class constraints

-- | Obtain the type variables mentioned in class constraints.
--
-- Example:
--
-- f(C a => C b => a -> b -> c) => {a,b}
constraintedTypeVariables ::
     Scheme Polymorphic
  -> Map (TypeVariable Polymorphic) (Set (ClassConstraint Polymorphic))
constraintedTypeVariables Scheme {constraints} =
  M.fromListWith
    (<>)
    (concatMap
       (\classConstraint@ClassConstraint {typ = types} ->
          [ (typeVariable, Set.singleton classConstraint)
          | typeVariable <- toList (foldMap typeVariables types)
          ])
       constraints)
  where
    typeVariables :: Type Polymorphic -> Set (TypeVariable Polymorphic)
    typeVariables =
      \case
        VariableType typeVariable -> Set.singleton typeVariable
        ApplyType TypeApplication {function, argument} ->
          typeVariables function <> typeVariables argument
        ConstantType {} -> mempty

--------------------------------------------------------------------------------
-- Find type variables which can be defaulted

-- | Produce the unique set of variables that may meaningfully be
-- defaulted for a cell.
--
-- I'm 90% sure that this is the right way to attack the problem of
-- defaulting cells. But I don't know of a precedent in the
-- literature/implementation world.
--
-- Examples:
--
-- (FromInteger a, FromDecimal b) => (a -> b) => {}
-- (FromInteger a, FromDecimal b) => {x: a, y: b} => {a,b}
-- (FromInteger a, FromDecimal b, FromDecimal c) => {x: a, y: c -> b} => {a}
-- (FromDecimal b, FromDecimal c) => {x: c, y: c -> b} => {c}
--
defaultableTypeVariables :: Scheme Polymorphic -> Set (TypeVariable Polymorphic)
defaultableTypeVariables Scheme {typ} = typeVariables typ
  where
    typeVariables =
      \case
        VariableType typeVariable -> Set.singleton typeVariable
        ApplyType TypeApplication {function, argument} ->
          case function of
            ConstantType TypeConstant {name = FunctionTypeName} ->
              mempty -- We ignore the whole function.
            _ -> typeVariables function <> typeVariables argument
        ConstantType {} -> mempty

--------------------------------------------------------------------------------
-- Substitution

data Substitution = Substitution
  { before :: !(TypeVariable Polymorphic)
  , after :: !(Type Polymorphic)
  } deriving (Show, Eq, Ord)

substituteClassConstraint ::
     Seq Substitution
  -> ClassConstraint Polymorphic
  -> ClassConstraint Polymorphic
substituteClassConstraint substitutions ClassConstraint {..} =
  ClassConstraint {typ = fmap (substituteType substitutions) typ, ..}

substituteType :: Seq Substitution -> Type Polymorphic -> Type Polymorphic
substituteType substitutions = go
  where
    go =
      \case
        typ@ConstantType {} -> typ
        ApplyType TypeApplication {function, argument, ..} ->
          ApplyType
            TypeApplication {function = go function, argument = go argument, ..}
        typ@(VariableType typeVariable :: Type Polymorphic) ->
          case find
                 (\Substitution {before} -> before == typeVariable)
                 substitutions of
            Just Substitution {after} -> after
            Nothing -> typ
