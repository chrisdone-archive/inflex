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

module Inflex.Resolver
  ( resolveText
  , resolveGeneralised
  , resolvePolyConstraint
  , module Inflex.Types.Resolver
  ) where

import           Control.Monad.State
import           Control.Monad.Validate
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Void
import           Inflex.Defaulter.Suggest
import           Inflex.Generaliser
import           Inflex.Location
import           Inflex.Type (expressionType, instanceNameType, typeOutput)
import           Inflex.Types
import           Inflex.Types.Resolver

--------------------------------------------------------------------------------
-- Top-level

resolveText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (GeneraliseResolveError e) (IsResolved (Expression Resolved))
resolveText globals fp text = do
  generalised <-
    first GeneraliserErrored (generaliseText globals fp text)
  resolveGeneralised generalised

resolveGeneralised ::
     IsGeneralised (Expression Generalised)
  -> Either (GeneraliseResolveError e) (IsResolved (Expression Resolved))
resolveGeneralised IsGeneralised {thing, polytype, mappings} = do
  (expression, ResolveState {implicits}) <-
    first
      ResolverErrors
      ((\(result, s) -> fmap (, s) result)
         (runState
            (runValidateT
               (runResolve (expressionResolver (DeBrujinNesting 0) thing)))
            ResolveState {implicits = mempty, defaulteds}))
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
  where
    allClassConstraints = expressionCollect thing
    typeVariableToConstraints =
      constraintsTypeVariablesGeneralised allClassConstraints
    defaulteds =
      M.mapMaybe
        (\classConstraints -> do
           constraints <- NE.nonEmpty (toList classConstraints)
           runIdentity (suggestTypeConstant constraints))
        typeVariableToConstraints

--------------------------------------------------------------------------------
-- Resolving expression tree

expressionResolver :: DeBrujinNesting -> Expression Generalised -> Resolve (Expression Resolved)
expressionResolver nesting =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (pure (literalResolver literal))
    HoleExpression hole ->
      fmap HoleExpression (pure (holeResolver hole))
    RecordExpression record ->
      fmap RecordExpression (recordResolver nesting record)
    PropExpression prop ->
      fmap PropExpression (propResolver nesting prop)
    ArrayExpression array ->
      fmap ArrayExpression (arrayResolver nesting array)
    VariantExpression variant ->
      fmap VariantExpression (variantResolver nesting variant)
    VariableExpression variable ->
      fmap VariableExpression (pure (variableResolver variable))
    LambdaExpression lambda ->
      fmap LambdaExpression (lambdaResolver nesting lambda)
    LetExpression let' -> fmap LetExpression (letResolver nesting let')
    ApplyExpression apply -> fmap ApplyExpression (applyResolver nesting apply)
    GlobalExpression global -> globalResolver nesting global
    InfixExpression infix' ->
      fmap InfixExpression (infixResolver nesting infix')

lambdaResolver :: DeBrujinNesting -> Lambda Generalised -> Resolve (Lambda Resolved)
lambdaResolver (DeBrujinNesting nesting) Lambda {..} = do
  body' <- expressionResolver (DeBrujinNesting (nesting + 1)) body
  pure Lambda {param = paramResolver param, body = body', ..}

letResolver :: DeBrujinNesting -> Let Generalised -> Resolve (Let Resolved)
letResolver (DeBrujinNesting nesting) Let {..} = do
  binds' <- traverse (bindResolver (DeBrujinNesting (nesting + 1))) binds
  body' <- expressionResolver (DeBrujinNesting (nesting + 1)) body
  pure Let {binds = binds', body = body', ..}

bindResolver :: DeBrujinNesting -> Bind Generalised -> Resolve (Bind Resolved)
bindResolver nesting Bind {..} = do
  let param' = paramResolver param
  value' <- expressionResolver nesting value
  pure Bind {param = param', value = value', ..}

applyResolver :: DeBrujinNesting -> Apply Generalised -> Resolve (Apply Resolved)
applyResolver nesting Apply {..} = do
  function' <- expressionResolver nesting function
  argument' <- expressionResolver nesting argument
  pure Apply {function = function', argument = argument', ..}

recordResolver :: DeBrujinNesting -> Record Generalised -> Resolve (Record Resolved)
recordResolver nesting Record {..} = do
  fields' <-
    traverse
      (\FieldE {location = l, ..} -> do
         e' <- expressionResolver nesting expression
         pure FieldE {location = l, expression = e', ..})
      fields
  pure Record {fields = fields', ..}

propResolver :: DeBrujinNesting -> Prop Generalised -> Resolve (Prop Resolved)
propResolver nesting Prop {..} = do
  expression' <- expressionResolver nesting expression
  pure Prop {expression = expression', ..}

arrayResolver :: DeBrujinNesting -> Array Generalised -> Resolve (Array Resolved)
arrayResolver nesting Array {..} = do
  expressions' <- traverse (expressionResolver nesting) expressions
  pure Array {expressions = expressions', ..}

variantResolver :: DeBrujinNesting -> Variant Generalised -> Resolve (Variant Resolved)
variantResolver nesting Variant {..} = do
  argument' <- traverse (expressionResolver nesting) argument
  pure Variant {argument = argument', ..}

variableResolver :: Variable Generalised -> Variable Resolved
variableResolver Variable {..} = Variable {..}

holeResolver :: Hole Generalised -> Hole Resolved
holeResolver Hole {..} = Hole {..}

literalResolver :: Literal Generalised -> Literal Resolved
literalResolver =
  \case
    NumberLiteral number -> NumberLiteral (numberResolver number)
    TextLiteral LiteralText {..} -> TextLiteral LiteralText {..}

numberResolver :: Number Generalised -> Number Resolved
numberResolver Number {..} = Number { ..}

paramResolver :: Param Generalised -> Param Resolved
paramResolver Param {..} = Param { ..}

infixResolver :: DeBrujinNesting -> Infix Generalised -> Resolve (Infix Resolved)
infixResolver nesting Infix {..} = do
  global' <- globalResolver nesting global
  left' <- expressionResolver nesting left
  right' <- expressionResolver nesting right
  pure Infix {global = global', left = left', right = right', ..}

--------------------------------------------------------------------------------
-- Adding implicit arguments to a global reference

globalResolver :: DeBrujinNesting -> Global Generalised -> Resolve (Expression Resolved)
globalResolver nesting global@Global {scheme = GeneralisedScheme Scheme {constraints}} = do
  implicits <-
    traverse
      (\constraint -> do
         defaulteds <- gets defaulteds
         case resolveGeneralisedConstraint defaulteds constraint of
           Left err -> Resolve (refute (pure err))
           Right resolution -> do
             case resolution of
               NoInstanceButPoly classConstraint -> do
                 deBrujinOffset <- addImplicitConstraint classConstraint
                 pure (DeferredDeBrujin deBrujinOffset)
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
                 DeferredDeBrujin offset ->
                   VariableExpression
                     Variable
                       { location = ImplicitArgumentFor location
                       , name = deBrujinIndex nesting offset
                       , typ = typ -- TODO: Check that this makes sense. [low prio]
                       }
           , typ = typeOutput (expressionType inner)
           })
    (GlobalExpression Global {scheme = ResolvedScheme typ, name = refl, ..})
    implicitArgs
  where
    Global {scheme = GeneralisedScheme Scheme {typ}, location, ..} = global
    refl =
      case name of
        HashGlobal g -> HashGlobal g
        FromIntegerGlobal -> FromIntegerGlobal
        EqualGlobal -> EqualGlobal
        FromDecimalGlobal -> FromDecimalGlobal
        NumericBinOpGlobal n -> NumericBinOpGlobal n
        FunctionGlobal f -> FunctionGlobal f

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
  defaulteds <- gets defaulteds
  case elemIndex classConstraint (toList implicits) of
    Nothing -> do
      let implicits' = implicits :|> classConstraint
      put (ResolveState {implicits = implicits', defaulteds}) -- Must be added to the END.
      pure (DeBrujinOffset (length implicits' - 1))
    Just index -> pure (DeBrujinOffset index)

-- | Given the current level of lambda nesting, offseted by the number
-- of implicit arguments, calculate the proper de Brujin index.
deBrujinIndex :: DeBrujinNesting -> DeBrujinOffset -> DeBrujinIndex
deBrujinIndex (DeBrujinNesting nesting) (DeBrujinOffset offset) =
  DeBrujinIndex (DeBrujinNesting (nesting + offset))

--------------------------------------------------------------------------------
-- Instance resolution

-- | Resolve a class constraint. Ensures that generalised type is
-- polymorphic first.
resolveGeneralisedConstraint ::
     Map (TypeVariable Generalised) (Type Polymorphic)
  -> ClassConstraint Generalised
  -> Either ResolutionError ResolutionSuccess
resolveGeneralisedConstraint defaulteds constraint = do
  polymorphicConstraint <- classConstraintPoly defaulteds constraint
  resolvePolyConstraint polymorphicConstraint

-- | For resolving polymorphic constraints.
--
-- Currently, there is no instances list. We have no user-definable
-- instances or classes. Therefore it's a trivial piece of logic to check that:
--
-- * An Integer type matches with FromDecimal or FromInteger.
-- * A Decimal i type matches any FromDecimal j provided i<=j.
--
resolvePolyConstraint ::
     ClassConstraint Polymorphic -> Either ResolutionError ResolutionSuccess
resolvePolyConstraint polymorphicConstraint@ClassConstraint {typ, className} = do
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
    [numberType]
      | Just numericBinOp <- classNameToNumericBinOp className ->
        fmap
          InstanceFound
          (resolveNumericBinOp numericBinOp className numberType)
    _ -> Left UnsupportedInstanceHead

-- | Given a class constraint, produce the operation that would be run
-- eventually. Immediately, we put it in the InstanceName, either
-- IntegerOpInstance or DecimalOpInstance.
classNameToNumericBinOp :: ClassName -> Maybe NumericBinOp
classNameToNumericBinOp =
  \case
    MulitplyOpClassName -> pure MulitplyOp
    AddOpClassName -> pure AddOp
    SubtractOpClassName -> pure SubtractOp
    DivideOpClassName -> pure DivideOp
    _ -> Nothing

-- | Make sure the type is really numeric, then produce the right
-- DecimalOpInstance or IntegerOpInstance.
resolveNumericBinOp ::
     NumericBinOp
  -> ClassName
  -> Type Polymorphic
  -> Either ResolutionError InstanceName
resolveNumericBinOp numericBinOp className =
  \case
    ConstantType TypeConstant {name = IntegerTypeName} ->
      pure (IntegerOpInstance numericBinOp)
    ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                              , argument = ConstantType TypeConstant {name = NatTypeName places}
                              } -> pure (DecimalOpInstance places numericBinOp)
    numberType -> Left (NoInstanceForType className numberType)

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

-- Here is where we could do defaulting.
--
-- A key factor is making sure that 2.3 + 3.45 defaults to Decimal 2,
-- not Decimal 1.
--
-- So we should re-use the logic from Inflex.Defaulter.

-- | Make sure the class constraint is polymorphic.
classConstraintPoly ::
     Map (TypeVariable Generalised) (Type Polymorphic)
  -> ClassConstraint Generalised
  -> Either ResolutionError (ClassConstraint Polymorphic)
classConstraintPoly defaulteds ClassConstraint {typ, ..} =
  case traverse (constrainPolymorphic defaulteds) typ of
    Left typeVariables -> Left (NoInstanceAndMono className typeVariables)
    Right polyTypes -> pure ClassConstraint {typ = polyTypes, ..}

-- | Constraint the type to remove monomorphic variables into a
-- polymorphic type. If there are any monomorphic variables, returns
-- Left with that variable.
constrainPolymorphic ::
     Map (TypeVariable Generalised) (Type Polymorphic)
  -> Type Generalised
  -> Either (TypeVariable Generalised) (Type Polymorphic)
constrainPolymorphic defaulteds = go
  where
    go =
      \case
        FreshType v -> absurd v
        RecordType t -> fmap RecordType (go t)
        VariantType t -> fmap VariantType (go t)
        ArrayType t -> fmap ArrayType (go t)
        VariableType typeVariable ->
          case M.lookup typeVariable defaulteds of
            Just type' -> pure type'
            Nothing -> Left typeVariable
        ApplyType TypeApplication {function, argument, location, kind} -> do
          function' <- go function
          argument' <- go argument
          pure
            (ApplyType
               TypeApplication
                 {function = function', argument = argument', location, kind})
        ConstantType TypeConstant {..} -> pure (ConstantType TypeConstant {..})
        PolyType typeVariable -> pure (VariableType typeVariable)
        RowType TypeRow {..} -> do
          typeVariable' <- maybe (pure Nothing) Left typeVariable
          fields' <- traverse fieldSolve fields
          pure
            (RowType
               TypeRow {fields = fields', typeVariable = typeVariable', ..})
    fieldSolve Field {..} = do
      typ' <- go typ
      pure Field {typ = typ', ..}

--------------------------------------------------------------------------------
-- Collect constraints

expressionCollect :: Expression Generalised -> Set (ClassConstraint Generalised)
expressionCollect =
  \case
    LiteralExpression {} -> mempty
    PropExpression prop -> propCollect prop
    HoleExpression {} -> mempty
    ArrayExpression array -> arrayCollect array
    VariantExpression variant -> variantCollect variant
    RecordExpression record -> recordCollect record
    LambdaExpression lambda -> lambdaCollect lambda
    LetExpression let' -> letCollect let'
    InfixExpression infix' -> infixCollect infix'
    ApplyExpression apply -> applyCollect apply
    VariableExpression {} -> mempty
    GlobalExpression global -> globalCollect global

globalCollect :: Global Generalised -> Set (ClassConstraint Generalised)
globalCollect Global {scheme = GeneralisedScheme scheme, ..} =
  collectScheme scheme

collectScheme :: Scheme Generalised -> Set (ClassConstraint Generalised)
collectScheme Scheme {..} = Set.fromList constraints

recordCollect :: Record Generalised -> Set (ClassConstraint Generalised)
recordCollect Record {..} =
  mconcat (map (\FieldE {expression} -> expressionCollect expression) fields)

propCollect :: Prop Generalised -> Set (ClassConstraint Generalised)
propCollect Prop {..} = expressionCollect expression

arrayCollect :: Array Generalised -> Set (ClassConstraint Generalised)
arrayCollect Array {..} = foldMap expressionCollect expressions

variantCollect :: Variant Generalised -> Set (ClassConstraint Generalised)
variantCollect Variant {..} = foldMap expressionCollect argument

lambdaCollect :: Lambda Generalised -> Set (ClassConstraint Generalised)
lambdaCollect Lambda {..} = expressionCollect body

letCollect :: Let Generalised -> Set (ClassConstraint Generalised)
letCollect Let {..} =
  mconcat (toList (fmap (bindCollect) binds)) <> expressionCollect body

infixCollect :: Infix Generalised -> Set (ClassConstraint Generalised)
infixCollect Infix {..} =
  expressionCollect left <> expressionCollect right <> globalCollect global

bindCollect :: Bind Generalised -> Set (ClassConstraint Generalised)
bindCollect Bind {..} = expressionCollect value

applyCollect :: Apply Generalised -> Set (ClassConstraint Generalised)
applyCollect Apply {..} =
  expressionCollect function <> expressionCollect argument
