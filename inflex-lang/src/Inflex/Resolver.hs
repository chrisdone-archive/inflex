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
import           Data.Foldable
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
data ImplicitArgument
  = InstanceFound InstanceName
  | NoInstanceButPoly (ClassConstraint Polymorphic)
  deriving (Show, Eq)

-- 1. The user put 2.52 when accuracy was 0.0.
-- 2. Unsupported instance head.
-- 3. No instance was found with monotypes.
-- 4. Invalid type for class instance heads.
data ResolutionError
  = LiteralDecimalPrecisionMismatch PrecisionMismatch
  | UnsupportedInstanceHead
  | NoInstanceAndMono (TypeVariable Generalised)
  | NoInstanceForType ClassName (Type Polymorphic)
  deriving (Show, Eq)

data PrecisionMismatch = PrecisionMismatch
  { supersetPlaces :: !Natural
  , subsetPlaces :: !Natural
  , constraint :: !(ClassConstraint Polymorphic)
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

data ResolveState = ResolveState
  { implicits :: !(Seq (ClassConstraint Polymorphic))
    -- ^ Each implicit constraint is added to the end of the sequence.

    -- The de Brujin index is calculated as current_deBurjin_index + 1
    -- + offset.  Where offset = index (zero-based, left to right)
    -- from this implicits list.

    -- The same implicit argument may be used more than once, so a
    -- lookup is performed first.
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
      }

--------------------------------------------------------------------------------
-- Resolving expression tree

expressionResolver :: Expression Generalised -> Resolve (Expression Resolved)
expressionResolver =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (pure (literalResolver literal))
    VariableExpression variable ->
      fmap VariableExpression (pure (variableResolver variable))
    LambdaExpression lambda -> fmap LambdaExpression (lambdaResolver lambda)
    ApplyExpression apply -> fmap ApplyExpression (applyResolver apply)
    GlobalExpression global -> globalResolver global

lambdaResolver :: Lambda Generalised -> Resolve (Lambda Resolved)
lambdaResolver Lambda {..} = do
  body' <- expressionResolver body
  pure Lambda {param = paramResolver param, body = body', ..}

applyResolver :: Apply Generalised -> Resolve (Apply Resolved)
applyResolver Apply {..} = do
  function' <- expressionResolver function
  argument' <- expressionResolver argument
  pure Apply {function = function', argument = argument', ..}

variableResolver :: Variable Generalised -> Variable Resolved
variableResolver Variable {..} = Variable {..}

literalResolver :: Literal Generalised -> Literal Resolved
literalResolver =
  \case
    NumberLiteral number -> NumberLiteral (numberResolver number)

numberResolver :: Number Generalised -> Number Resolved
numberResolver Number {..} = Number { ..}

paramResolver :: Param Generalised -> Param Resolved
paramResolver Param {..} = Param { ..}

globalResolver :: Global Generalised -> Resolve (Expression Resolved)
globalResolver global@Global {scheme = GeneralisedScheme Scheme {constraints}} = do
  implicits <-
    traverse
      (\constraint ->
         case resolveConstraint constraint of
           Left err -> Resolve (refute (pure err))
           Right resolution -> do
             case resolution of
               NoInstanceButPoly{} -> undefined
               _ -> pure ()
             pure resolution)
      constraints
  pure (addImplicitsToGlobal implicits global)

--------------------------------------------------------------------------------
-- Adding implicit arguments to a global reference

addImplicitsToGlobal ::
     [ImplicitArgument] -> Global Generalised -> Expression Resolved
addImplicitsToGlobal implicits global =
  foldl
    (\inner implicit ->
       ApplyExpression
         Apply
           { location = ImplicitlyApplicationOn location
           , function = inner
           , argument =
               case implicit of
                 InstanceFound instanceName ->
                   GlobalExpression
                     Global
                       { location = ImplicitArgumentFor location
                       , name = InstanceGlobal instanceName
                       , scheme = undefined
                       }
                 NoInstanceButPoly _ -> undefined
           , typ = undefined
           })
    (GlobalExpression Global {scheme = ResolvedScheme scheme, ..})
    implicits
  where
    Global {scheme = GeneralisedScheme scheme, location, ..} = global

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
     ClassConstraint Generalised -> Either ResolutionError ImplicitArgument
resolveConstraint constraint = do
  polymorphicConstraint@ClassConstraint {typ, className} <-
    classConstraintPoly constraint
  case toList typ of
    (VariableType {}:_) -> pure (NoInstanceButPoly polymorphicConstraint)
    [_, VariableType {}] -> pure (NoInstanceButPoly polymorphicConstraint)
    [numberType]
      | className == FromIntegerClassName ->
        fmap InstanceFound (resolveFromInteger numberType)
    [ConstantType places, numberType]
      | className == FromDecimalClassName ->
        fmap
          InstanceFound
          (resolveFromDecimal places numberType polymorphicConstraint)
    _ -> Left UnsupportedInstanceHead

-- | Resolve an instance of FromInteger for a given type.
resolveFromInteger ::
     Type Polymorphic -> Either ResolutionError InstanceName
resolveFromInteger numberType =
  case numberType of
    ConstantType TypeConstant {name = IntegerTypeName} ->
      pure FromIntegerIntegerInstance
    ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                              , argument = ConstantType TypeConstant {name = NatTypeName places}
                              } -> pure (FromIntegerDecimalInstance places)
    _ -> Left (NoInstanceForType FromIntegerClassName numberType)

-- | Resolve an instance of FromDecimal for a given type.
resolveFromDecimal ::
     TypeConstant Polymorphic
  -> Type Polymorphic
  -> ClassConstraint Polymorphic
  -> Either ResolutionError InstanceName
resolveFromDecimal TypeConstant {name = natural} numericType constraint =
  case (natural, numericType) of
    (NatTypeName supersetPlaces, ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                                                           , argument = ConstantType TypeConstant {name = NatTypeName subsetPlaces}
                                                           }) ->
      if supersetPlaces >= subsetPlaces
        then pure
               (FromDecimalDecimalInstance
                  FromDecimalInstance {supersetPlaces, subsetPlaces})
        else Left
               (LiteralDecimalPrecisionMismatch
                  PrecisionMismatch {subsetPlaces, supersetPlaces, constraint})
    _ -> Left (NoInstanceForType FromDecimalClassName numericType)

--------------------------------------------------------------------------------
-- Polymorphization

-- | Make sure the class constraint is polymorphic.
classConstraintPoly ::
     ClassConstraint Generalised
  -> Either ResolutionError (ClassConstraint Polymorphic)
classConstraintPoly ClassConstraint {typ, ..} =
  case traverse constrainPolymorphic typ of
    Left typeVariables -> Left (NoInstanceAndMono typeVariables)
    Right polyTypes -> pure ClassConstraint {typ = polyTypes, ..}

-- | Constraint the type to remove monomorphic variables into a
-- polymorphic type. If there are any monomorphic variables, returns
-- Left with that variable.
constrainPolymorphic ::
     Type Generalised -> Either (TypeVariable Generalised) (Type Polymorphic)
constrainPolymorphic = go
  where
    go =
      \case
        VariableType typeVariable -> Left typeVariable
        ApplyType TypeApplication {function, argument, location, kind} -> do
          function' <- go function
          argument' <- go argument
          pure
            (ApplyType
               TypeApplication
                 {function = function', argument = argument', location, kind})
        ConstantType TypeConstant {..} ->
          pure (ConstantType TypeConstant {..})
        PolyType typeVariable -> pure (VariableType typeVariable)
