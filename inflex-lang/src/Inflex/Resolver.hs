{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Inflex.Generaliser
import Inflex.Location
import Inflex.Type (expressionType, instanceNameType, typeOutput)
import Inflex.Types
import Numeric.Natural

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
  | UnsupportedInstanceHead
  | NoInstanceAndMono (TypeVariable Generalised)
  | NoInstanceForType ClassName (Type Polymorphic)
  deriving (Show, Eq)

-- An implicit argument.
data ImplicitArgument
  = ExactInstance InstanceName
  | DeferredDeBrujin DeBrujinOffset
                     (ClassConstraint Polymorphic)
  deriving (Show, Eq)

newtype DeBrujinNesting =
  DeBrujinNesting Int
  deriving (Show, Eq, Ord)

newtype DeBrujinOffset = DeBrujinOffset
  { unDeBrujinOffset :: Int
  } deriving (Show, Eq, Ord)

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
  (expression, ResolveState {implicits}) <-
    first
      ResolverErrors
      ((\(result, s) -> fmap (, s) result)
         (runState
            (runValidateT
               (runResolve (expressionResolver (DeBrujinNesting 0) thing)))
            ResolveState {implicits = mempty}))
  pure
    IsResolved
      { mappings
      , thing = addImplicitParamsToExpression expression implicits
      , scheme =
          Scheme
            { location = expressionLocation thing
            -- The reverse is intentional. See documentation of ResolveState.
            , constraints = reverse (toList implicits)
            , typ = polytype
            }
      }

--------------------------------------------------------------------------------
-- Resolving expression tree

expressionResolver :: DeBrujinNesting -> Expression Generalised -> Resolve (Expression Resolved)
expressionResolver nesting =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (pure (literalResolver literal))
    VariableExpression variable ->
      fmap VariableExpression (pure (variableResolver variable))
    LambdaExpression lambda -> fmap LambdaExpression (lambdaResolver nesting lambda)
    ApplyExpression apply -> fmap ApplyExpression (applyResolver nesting apply)
    GlobalExpression global -> globalResolver nesting global

lambdaResolver :: DeBrujinNesting -> Lambda Generalised -> Resolve (Lambda Resolved)
lambdaResolver (DeBrujinNesting nesting) Lambda {..} = do
  body' <- expressionResolver (DeBrujinNesting (nesting + 1)) body
  pure Lambda {param = paramResolver param, body = body', ..}

applyResolver :: DeBrujinNesting -> Apply Generalised -> Resolve (Apply Resolved)
applyResolver nesting Apply {..} = do
  function' <- expressionResolver nesting function
  argument' <- expressionResolver nesting argument
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

--------------------------------------------------------------------------------
-- Adding implicit arguments to a global reference

globalResolver :: DeBrujinNesting -> Global Generalised -> Resolve (Expression Resolved)
globalResolver nesting global@Global {scheme = GeneralisedScheme Scheme {constraints}} = do
  implicits <-
    traverse
      (\constraint ->
         case resolveConstraint constraint of
           Left err -> Resolve (refute (pure err))
           Right resolution -> do
             case resolution of
               NoInstanceButPoly classConstraint -> do
                 deBrujinOffset <- addImplicitConstraint classConstraint
                 pure (DeferredDeBrujin deBrujinOffset classConstraint)
               InstanceFound instanceName -> pure (ExactInstance instanceName))
      constraints
  pure (addImplicitArgsToGlobal nesting implicits global)

-- | Wrap implicit arguments around a global reference.
addImplicitArgsToGlobal ::
     DeBrujinNesting -- ^ Current deBrujin nesting level.
  -> [ImplicitArgument] -- ^ Implicit arguments to wrap.
  -> Global Generalised -- ^ Global that accepts implicit arguments.
  -> Expression Resolved
addImplicitArgsToGlobal nesting implicitArgs global =
  foldl -- TODO: Check foldl vs foldr ordering!
    (\inner implicit ->
       ApplyExpression
         Apply
           { location = ImplicitlyApplicationOn location
           , function = inner
           , argument =
               case implicit of
                 ExactInstance instanceName ->
                   GlobalExpression
                     Global
                       { location = ImplicitArgumentFor location
                       , name = InstanceGlobal instanceName
                       , scheme = ResolvedScheme (instanceNameType instanceName)
                       }
                 -- TODO: Drop the _classConstraint from the type?
                 DeferredDeBrujin offset _classConstraint ->
                   VariableExpression
                     Variable
                       { location = ImplicitArgumentFor location
                       , name = deBrujinIndex nesting offset
                       , typ = typ -- TODO: Check that this makes sense.
                       }
           , typ = typeOutput (expressionType inner)
           })
    (GlobalExpression Global {scheme = ResolvedScheme typ, ..})
    implicitArgs
  where
    Global {scheme = GeneralisedScheme Scheme {typ}, location, ..} = global

-- | Add implicit parameters to an expression.
--
-- We wrap it in one lambda per implicit argument.
addImplicitParamsToExpression ::
     Expression Resolved
  -> Seq (ClassConstraint Polymorphic)
  -> Expression Resolved
addImplicitParamsToExpression =
  foldl -- TODO: Check left vs right.
    (\body ClassConstraint {location} ->
       LambdaExpression
         Lambda
           { location = ImplicitArgumentFor location
           , param =
               Param
                 { location = ImplicitArgumentFor location
                 , name = ()
                 , typ = expressionType body -- TODO: Make accurate. Low prio.
                 }
           , body
           , typ = expressionType body -- TODO: Make accurate. Low prio.
           })

-- | Add an implicit constraint to the top-level. Duplicates are no-op.
addImplicitConstraint :: ClassConstraint Polymorphic -> Resolve DeBrujinOffset
addImplicitConstraint classConstraint = do
  implicits <- gets implicits
  case elemIndex classConstraint (toList implicits) of
    Nothing -> do
      let implicits' = implicits :|> classConstraint
      put (ResolveState {implicits = implicits'}) -- Must be added to the END.
      pure (DeBrujinOffset (length implicits' - 1))
    Just index -> pure (DeBrujinOffset index)

-- | Given the current level of lambda nesting, offseted by the number
-- of implicit arguments, calculate the proper de Brujin index.
deBrujinIndex :: DeBrujinNesting -> DeBrujinOffset -> DeBrujinIndex
deBrujinIndex (DeBrujinNesting nesting) (DeBrujinOffset offset) =
  DeBrujinIndex (nesting + offset)

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
    (NatTypeName subsetPlaces, ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                                                           , argument = ConstantType TypeConstant {name = NatTypeName supersetPlaces}
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
