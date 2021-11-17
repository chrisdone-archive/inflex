{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Type generator for Inflex.

module Inflex.Generator
  ( generateText
  , generateRenamed
  , RenameGenerateError(..)
  , HasConstraints(..)
  , hasConstraintsMappingsL
  , generateVariableType
  , generateTypeVariable
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Validate
import           Data.Bifunctor
import           Data.Decimal
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Data.Traversable
import           Data.Validation
import           Data.Void
import           Inflex.Filler
import           Inflex.Instances ()
import           Inflex.Location
import qualified Inflex.Renamer as Renamer
import           Inflex.Type
import           Inflex.Types
import qualified Inflex.Types as Alternative (Alternative(..))
import qualified Inflex.Types as Bind (Bind(..))
import qualified Inflex.Types as Field (FieldE(..))
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import           Optics hiding (Fold)

--------------------------------------------------------------------------------
-- Top-level

generateText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (RenameGenerateError e) (HasConstraints (Expression Generated))
generateText globals fp text = do
  isrenamed <- first RenameGenerateError (Renamer.renameText fp text)
  generateRenamed
    globals
    emptyFillerEnv
    isrenamed

generateRenamed ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FillerEnv e
  -> Renamer.IsRenamed (Expression Renamed)
  -> Either (RenameGenerateError e) (HasConstraints (Expression Generated))
generateRenamed globalTypes globalNames Renamer.IsRenamed { thing = expressionRenamed
                                                          , mappings
                                                          } = do
  expression <-
    first
      FillErrors
      (toEither (runFiller (expressionFill globalNames expressionRenamed)))
  generateFilled globalTypes expression mappings

generateFilled ::
     Map Hash (Either e (Scheme Polymorphic))
  -> Expression Filled
  -> Map Cursor SourceLocation
  -> Either (RenameGenerateError e) (HasConstraints (Expression Generated))
generateFilled globalTypes expression mappings =
  first
    GeneratorErrors
    (let (result, GenerateState {equalityConstraints}) =
           runState
             (runReaderT
                (runValidateT (runGenerator (expressionGenerator expression)))
                (Env {globals = globalTypes, scope = mempty}))
             GenerateState {counter = 0, equalityConstraints = mempty}
      in fmap
           (\thing ->
              HasConstraints {thing, mappings, equalities = equalityConstraints})
           result)

--------------------------------------------------------------------------------
-- Generators

expressionGenerator :: Expression Filled -> Generate e (Expression Generated)
expressionGenerator =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (literalGenerator literal)
    PropExpression prop ->
      fmap PropExpression (propGenerator prop)
    ArrayExpression array ->
      fmap ArrayExpression (arrayGenerator array)
    VariantExpression variant ->
      fmap VariantExpression (variantGenerator variant)
    RecordExpression record ->
      fmap RecordExpression (recordGenerator record)
    LambdaExpression lambda ->
      fmap LambdaExpression (lambdaGenerator lambda)
    LetExpression let' ->
      fmap LetExpression (letGenerator let')
    CaseExpression case' ->
      fmap CaseExpression (caseGenerator case')
    FoldExpression fold' ->
      fmap FoldExpression (foldGenerator fold')
    UnfoldExpression unfold' ->
      fmap UnfoldExpression (unfoldGenerator unfold')
    IfExpression if' ->
      fmap IfExpression (ifGenerator if')
    InfixExpression infix' ->
      fmap InfixExpression (infixGenerator infix')
    ApplyExpression apply ->
      fmap ApplyExpression (applyGenerator apply)
    VariableExpression variable ->
      fmap VariableExpression (variableGenerator variable)
    GlobalExpression global ->
      fmap GlobalExpression (globalGenerator global)
    HoleExpression hole ->
      fmap HoleExpression (holeGenerator hole)

caseGenerator :: Case Filled -> Generate e (Case Generated)
caseGenerator Case {..} = do
  scrutinee' <- expressionGenerator scrutinee
  alternatives' <- traverse alternativeGenerator alternatives
  commonResult <- generateVariableType location CasePrefix TypeKind
  traverse_
    (\Alternative {expression, pattern'} -> do
       addEqualityConstraint
         EqualityConstraint
           {location, type1 = commonResult, type2 = expressionType expression}
       patternType' <- patternType pattern'
       addEqualityConstraint
         EqualityConstraint
           {location, type1 = expressionType scrutinee', type2 = patternType'})
    alternatives'
  scrutineeType <-
    do rowVariable <- generateTypeVariable location AltPrefix RowKind
       pure
         (VariantType
            (RowType
               (TypeRow
                  { location
                  , typeVariable =
                      case find
                             (\Alternative {pattern'} ->
                                case pattern' of
                                  ParamPattern {} -> True
                                  WildPattern {} -> True
                                  _ -> False)
                             alternatives' of
                        Just {} -> Just rowVariable
                        Nothing -> Nothing
                  , fields =
                      [ Field
                        { location
                        , name = FieldName name
                        , typ = maybe (nullType location) paramType argument
                        }
                      | Alternative {pattern' = VariantPattern VariantP { tag = TagName name
                                                                        , argument
                                                                        }} <-
                          toList alternatives'
                      ]
                  })))
  addEqualityConstraint
    EqualityConstraint
      {location, type1 = expressionType scrutinee', type2 = scrutineeType}
  pure
    Case
      { scrutinee = scrutinee'
      , alternatives = alternatives'
      , typ = commonResult
      , ..
      }

foldGenerator :: Fold Filled -> Generate e (Fold Generated)
foldGenerator Fold {..} = do
  expression' <- expressionGenerator expression
  pure Fold {expression = expression', typ = undefined, ..}

unfoldGenerator :: Unfold Filled -> Generate e (Unfold Generated)
unfoldGenerator Unfold {..} = do
  expression' <- expressionGenerator expression
  pure Unfold {expression = expression', typ = undefined, ..}

patternType :: Pattern Generated -> Generate e (Type Generated)
patternType =
  \case
    ParamPattern p -> pure (paramType p)
    VariantPattern variantP -> variantPType variantP
    WildPattern hole -> pure (holeType hole)

variantPType :: VariantP Generated -> Generate e (Type Generated)
variantPType VariantP {tag = TagName name, argument, location} = do
  rowVariable <- generateTypeVariable location AltPrefix RowKind
  pure
    (VariantType
       (RowType
          (TypeRow
             { location
             , typeVariable = Just rowVariable
             , fields =
                 [ Field
                     { location
                     , name = FieldName name
                     , typ = maybe (nullType location) paramType argument
                     }
                 ]
             })))

alternativeGenerator :: Alternative Filled -> Generate e (Alternative Generated)
alternativeGenerator Alternative {..} = do
  pattern'' <- patternGenerator pattern'
  let addParam =
        case Renamer.patternParam pattern'' of
          Nothing -> id
          Just param -> over envScopeL (CaseBinding param :)
  expression' <- local addParam (expressionGenerator expression)
  pure Alternative {pattern' = pattern'', expression = expression', ..}

patternGenerator :: Pattern Filled -> Generate e (Pattern Generated)
patternGenerator =
  \case
    ParamPattern param -> fmap ParamPattern (paramGenerator param)
    VariantPattern variantP -> fmap VariantPattern (variantPGenerator variantP)
    WildPattern wildP -> fmap WildPattern (wildPGenerator wildP)

wildPGenerator :: Hole Filled -> Generate e (Hole Generated)
wildPGenerator Hole {..} = do
  generated <- generateVariableType location AltPrefix TypeKind
  pure Hole {typ = generated, ..}

variantPGenerator :: VariantP Filled -> Generate e (VariantP Generated)
variantPGenerator VariantP {..} = do
  argument' <- traverse paramGenerator argument
  pure VariantP {argument = argument', ..}

ifGenerator :: If Filled -> Generate e (If Generated)
ifGenerator If {..} = do
  condition' <- expressionGenerator condition
  consequent' <- expressionGenerator consequent
  alternative' <- expressionGenerator alternative
  common <- generateVariableType location IfPrefix TypeKind
  addEqualityConstraint
    EqualityConstraint
      {location, type1 = boolType location, type2 = expressionType condition'}
  addEqualityConstraint
    EqualityConstraint
      {location, type1 = common, type2 = expressionType consequent'}
  addEqualityConstraint
    EqualityConstraint
      {location, type1 = common, type2 = expressionType alternative'}
  pure
    If
      { consequent = consequent'
      , condition = condition'
      , alternative = alternative'
      , typ = common
      , ..
      }

holeGenerator :: Hole Filled -> Generate e (Hole Generated)
holeGenerator Hole {..} = do
  elementVariable <- generateVariableType location HolePrefix TypeKind
  pure Hole {typ = elementVariable, ..}

recordGenerator :: Record Filled -> Generate e (Record Generated)
recordGenerator Record {..} = do
  fields' <-
    traverse
      (\FieldE {location = l, ..} -> do
         expression' <- expressionGenerator expression
         pure FieldE {expression = expression', location = l, ..})
      fields
  let rowType =
        RowType
          TypeRow
            { location
            , typeVariable = Nothing
            , fields =
                map
                  (\FieldE {location = l, ..} ->
                     Field {typ = expressionType expression, location = l, ..})
                  fields'
            }
  pure Record {typ = RecordType rowType, fields = fields', ..}

propGenerator :: Prop Filled -> Generate e (Prop Generated)
propGenerator Prop {..} = do
  rowVariable <- generateTypeVariable location RowVarPrefix RowKind
  expression' <- expressionGenerator expression
  fieldType <- generateVariableType location FieldTypePrefix TypeKind
  let rowType =
        RowType
          TypeRow
            { typeVariable = pure rowVariable
            , fields = [Field {location, name, typ = fieldType}]
            , ..
            }
  addEqualityConstraint
    EqualityConstraint
      {type1 = RecordType rowType, type2 = expressionType expression', ..}
  pure Prop {typ = fieldType, expression = expression', ..}

arrayGenerator :: Array Filled -> Generate e (Array Generated)
arrayGenerator Array {..} = do
  elementVariable <- generateVariableType location ArrayElementPrefix TypeKind
  case typ of
    Nothing -> pure ()
    Just typ' -> do
      fullTypeSig <- renamedToGenerated typ'
      addEqualityConstraint
        EqualityConstraint
          {type1 = fullTypeSig, type2 = ArrayType elementVariable, ..}
  expressions' <-
    traverse
      (\e -> do
         e' <- expressionGenerator e
         addEqualityConstraint
           EqualityConstraint
             {type2 = elementVariable, type1 = expressionType e', ..}
         pure e')
      expressions
  pure Array {typ = ArrayType elementVariable, expressions = expressions', ..}

variantGenerator :: Variant Filled -> Generate e (Variant Generated)
variantGenerator Variant {typ = mtyp, ..} = do
  rowVariable <- generateTypeVariable location VariantRowVarPrefix RowKind
  argument' <- traverse expressionGenerator argument
  let rowType =
        RowType
          TypeRow
            { location
            , typeVariable = Just rowVariable
            , fields =
                [ Field
                    { location
                    , name = FieldName (unTagName tag)
                    , typ = maybe (nullType location) expressionType argument'
                    }
                ]
            }
      typ = VariantType rowType
  for_
    mtyp
    (\ty0 -> do
       ty <- renamedToGenerated ty0
       addEqualityConstraint
         EqualityConstraint {location, type1 = ty, type2 = typ})
  pure Variant {typ, argument = argument', ..}

literalGenerator :: Literal Filled -> Generate e (Literal Generated)
literalGenerator =
  \case
    NumberLiteral number -> fmap NumberLiteral (numberGenerator number)
    TextLiteral LiteralText {..} ->
      pure (TextLiteral LiteralText {typ = literalTextType location, ..})

-- | Produce a immediately-known concrete type
literalTextType ::
     StagedLocation Filled -> Type Generated
literalTextType location =
      ConstantType TypeConstant {location, name = TextTypeName}

numberGenerator :: Number Filled -> Generate e (Number Generated)
numberGenerator Number {typ = _, ..} =
  pure Number {typ = someNumberType location number, ..}

-- | Produce a immediately-known concrete type
someNumberType ::
     StagedLocation Filled -> SomeNumber -> Type Generated
someNumberType location =
  \case
    IntegerNumber {} ->
      ConstantType TypeConstant {location, name = IntegerTypeName}
    DecimalNumber Decimal {places} ->
      ApplyType
        TypeApplication
          { function =
              ConstantType TypeConstant {name = DecimalTypeName, location}
          , argument =
              ConstantType TypeConstant {name = NatTypeName places, location}
          , kind = TypeKind
          , ..
          }

lambdaGenerator :: Lambda Filled -> Generate e (Lambda Generated)
lambdaGenerator Lambda {typ = mtyp, ..} = do
  param' <- paramGenerator param
  body' <-
    local (over envScopeL (LambdaBinding param' :)) (expressionGenerator body)
  let outputType = expressionType body'
  let typ =
        ApplyType
          TypeApplication
            { function =
                ApplyType
                  TypeApplication
                    { function =
                        ConstantType
                          (TypeConstant {name = FunctionTypeName, location})
                    , argument = paramType param'
                    , location
                    , kind = FunKind TypeKind TypeKind
                    }
            , argument = outputType
            , location
            , kind = TypeKind
            }
  for
    mtyp
    (\ty0 -> do
       ty <- renamedToGenerated ty0
       addEqualityConstraint
         EqualityConstraint {location, type1 = ty, type2 = typ})
  pure Lambda {typ, body = body', param = param', ..}

letGenerator :: Let Filled -> Generate e (Let Generated)
letGenerator Let {typ = _, ..} = do
  binds' <- traverse bindGenerator binds
  body' <-
    local
      (over envScopeL (LetBinding (fmap (\Bind {param} -> param) binds') :))
      (expressionGenerator body)
  pure Let {body = body', binds = binds', typ = expressionType body', ..}

infixGenerator :: Infix Filled -> Generate e (Infix Generated)
infixGenerator Infix {typ = _, ..} = do
  ty <- generateVariableType location InfixOutputPrefix TypeKind
  global' <- globalGenerator global
  left' <- expressionGenerator left
  right' <- expressionGenerator right
  addEqualityConstraint
    EqualityConstraint
      { type1 = globalType global'
      , type2 = expressionType left' .-> expressionType right' .-> ty
      , ..
      }
  pure Infix {global = global', right = right', left = left', typ = ty, ..}

bindGenerator :: Bind Filled -> Generate e (Bind Generated)
bindGenerator Bind {..} = do
  param' <- paramGenerator param
  value' <- expressionGenerator value
  addEqualityConstraint
    EqualityConstraint
      {type1 = paramType param', type2 = expressionType value', ..}
  pure Bind {param = param', value = value', typ = paramType param', ..}

paramGenerator :: Param Filled -> Generate e (Param Generated)
paramGenerator Param {typ = _, ..} = do
  typ <- generateVariableType location LambdaParameterPrefix TypeKind
  pure Param {typ, ..}

applyGenerator :: Apply Filled -> Generate e (Apply Generated)
applyGenerator Apply {..} = do
  function' <- expressionGenerator function
  argument' <- expressionGenerator argument
  outputType <-
    case typ of
      Nothing ->
        generateVariableType (expressionLocation argument') ApplyPrefix TypeKind
      Just typ' -> renamedToGenerated typ'
  let functionTemplate =
        ApplyType
          TypeApplication
            { function =
                ApplyType
                  TypeApplication
                    { function =
                        ConstantType
                          (TypeConstant {name = FunctionTypeName, location})
                    , argument = expressionType argument'
                    , location
                    , kind = FunKind TypeKind TypeKind
                    }
            , argument = outputType
            , location = expressionLocation function'
            , kind = TypeKind
            }
  addEqualityConstraint
    EqualityConstraint
      {type1 = expressionType function', type2 = functionTemplate, ..}
  pure Apply {function = function', argument = argument', typ = outputType, ..}

variableGenerator :: Variable Filled -> Generate e (Variable Generated)
variableGenerator variable@Variable {typ = _, name = index, ..} = do
  Env {scope} <- ask
  case do binding <-
            lookup
              (deBrujinIndexNesting index)
              (zip (map DeBrujinNesting [0 ..]) scope)
          case binding of
            LambdaBinding param -> pure param
            CaseBinding param -> pure param
            LetBinding params
              | DeBrujinIndexOfLet _ (IndexInLet subIndex) <- index ->
                lookup subIndex (zip [0..] (toList params))
              | otherwise -> Nothing
             of
    Nothing -> Generate (refute (pure (MissingVariableG variable)))
    Just Param {typ = type2} -> do
      type1 <- generateVariableType location VariablePrefix TypeKind
      addEqualityConstraint EqualityConstraint {type1, type2, ..}
      pure Variable {typ = type1, name = index, ..}

globalGenerator :: Global Filled -> Generate e (Global Generated)
globalGenerator Global {name, location} = do
  scheme <-
    case name of
      FunctionGlobal function ->
        polymorphicSchemeToGenerated location (functionScheme location function)
      HashGlobal hash -> do
        Env {globals} <- ask
        case M.lookup hash globals of
          Nothing -> Generate (refute (pure (MissingHashG hash)))
          Just result ->
            case result of
              Left e -> Generate (refute (pure (OtherCellErrorG name e)))
              Right scheme -> polymorphicSchemeToGenerated location scheme
      NumericBinOpGlobal numericBinOp -> do
        a <- generateVariableType location IntegerPrefix TypeKind
        pure
          Scheme
            { constraints =
                [ ClassConstraint
                    { className = numericBinOpClassName numericBinOp
                    , typ = pure a
                    , ..
                    }
                ]
            , typ = a .-> a .-> a
            , ..
            }
      FromIntegerGlobal -> do
        typeVariable <- generateVariableType location IntegerPrefix TypeKind
        pure
          Scheme
            { constraints =
                [ ClassConstraint
                    { className = FromIntegerClassName
                    , typ = pure typeVariable
                    , ..
                    }
                ]
            , typ = integerT .-> typeVariable
            , ..
            }
      EqualGlobal _equality -> do
        typeVariable <- generateVariableType location EqualPrefix TypeKind
        pure
          Scheme
            { constraints =
                [ ClassConstraint
                    {className = EqualClassName, typ = pure typeVariable, ..}
                ]
            , typ = typeVariable .-> typeVariable .-> boolType location
            , ..
            }
      CompareGlobal _compareity -> do
        typeVariable <- generateVariableType location ComparePrefix TypeKind
        pure
          Scheme
            { constraints =
                [ ClassConstraint
                    {className = CompareClassName, typ = pure typeVariable, ..}
                ]
            , typ = typeVariable .-> typeVariable .-> boolType location
            , ..
            }
      FromDecimalGlobal -> do
        numberVar <- generateVariableType location DecimalPrefix TypeKind
        precisionVar <- generateVariableType location NatPrefix NatKind
        pure
          Scheme
            { constraints =
                [ ClassConstraint
                    { className = FromDecimalClassName
                    , typ = pure precisionVar <> pure numberVar
                    , ..
                    }
                ]
            , typ = decimalTVar precisionVar .-> numberVar
            , ..
            }
  pure Global {scheme = GeneratedScheme scheme, name = refl, ..}
  where
    refl =
      case name of
        FromIntegerGlobal -> FromIntegerGlobal
        FromDecimalGlobal -> FromDecimalGlobal
        NumericBinOpGlobal n -> NumericBinOpGlobal n
        HashGlobal h -> HashGlobal h
        EqualGlobal e -> EqualGlobal e
        CompareGlobal e -> CompareGlobal e
        FunctionGlobal f -> FunctionGlobal f

--------------------------------------------------------------------------------
-- Map from operators to classes

numericBinOpClassName :: NumericBinOp -> ClassName
numericBinOpClassName = \case
  MulitplyOp -> MulitplyOpClassName
  AddOp -> AddOpClassName
  SubtractOp -> SubtractOpClassName
  DivideOp -> DivideOpClassName

--------------------------------------------------------------------------------
-- Type system helpers

generateTypeVariable ::
     (MonadState GenerateState m)
  => StagedTyVarLocation s
  -> StagedPrefix s
  -> Kind
  -> m (TypeVariable s)
generateTypeVariable location prefix kind = do
  index <- gets (view generateStateCounterL)
  modify' (over generateStateCounterL succ)
  pure (TypeVariable {prefix, index, location, kind})

generateVariableType ::
     MonadState GenerateState m
  => StagedTyVarLocation s
  -> StagedPrefix s
  -> Kind
  -> m (Type s)
generateVariableType location prefix kind =
  fmap VariableType (generateTypeVariable location prefix kind)

addEqualityConstraint :: EqualityConstraint -> Generate e ()
addEqualityConstraint constraint =
  modify' (over generateStateEqualityConstraintsL (Seq.|> constraint))

--------------------------------------------------------------------------------
-- Generation of renamed type

renamedToGenerated :: Type Renamed -> Generate e (Type Generated)
renamedToGenerated =
  \case
    FreshType location -> generateVariableType location FreshPrefix TypeKind
    RecursiveType typ -> fmap RecursiveType (renamedToGenerated typ)
    DeBruijnType i -> pure (DeBruijnType i)
    RecordType t -> fmap RecordType (renamedToGenerated t)
    VariantType t -> fmap VariantType (renamedToGenerated t)
    ArrayType t -> fmap ArrayType (renamedToGenerated t)
    VariableType TypeVariable {..} -> pure (VariableType TypeVariable {..})
    RowType TypeRow {..} -> do
      fields' <- traverse fieldToGen fields
      typeVariable' <-
        for
          typeVariable
          (\() -> generateTypeVariable location RowVarPrefix RowKind)
      pure
        (RowType TypeRow {fields = fields', typeVariable = typeVariable', ..})
    ConstantType TypeConstant {..} -> pure (ConstantType TypeConstant {..})
    ApplyType TypeApplication {..} -> do
      function' <- renamedToGenerated function
      argument' <- renamedToGenerated argument
      pure
        (ApplyType
           TypeApplication {function = function', argument = argument', ..})
  where
    fieldToGen Field {..} = do
      typ' <- renamedToGenerated typ
      pure Field {typ = typ', ..}

--------------------------------------------------------------------------------
-- Convert a polymorphic scheme to a generated scheme

-- | This is where a polymorphic variable becomes monomorphic, with
-- each poly type becoming a unification type variable.
polymorphicSchemeToGenerated :: Cursor -> Scheme Polymorphic -> Generate e (Scheme Generated)
polymorphicSchemeToGenerated location0 = flip evalStateT mempty . rewriteScheme
  where
    rewriteScheme ::
         Scheme Polymorphic
      -> StateT (Map (TypeVariable Polymorphic) (TypeVariable Generated)) (Generate e) (Scheme Generated)
    rewriteScheme Scheme {typ, constraints, location} = do
      constraints' <- traverse rewriteConstraint constraints
      typ' <- rewriteType typ
      pure Scheme {location = location, typ = typ', constraints = constraints'}
    rewriteConstraint ::
         ClassConstraint Polymorphic
      -> StateT (Map (TypeVariable Polymorphic) (TypeVariable Generated)) (Generate e) (ClassConstraint Generated)
    rewriteConstraint ClassConstraint {..} = do
      typ' <- traverse rewriteType typ
      pure ClassConstraint {typ = typ', ..}
    rewriteType ::
         Type Polymorphic
      -> StateT (Map (TypeVariable Polymorphic) (TypeVariable Generated)) (Generate e) (Type Generated)
    rewriteType =
      \case
        FreshType v -> absurd v
        RecordType t -> fmap RecordType (rewriteType t)
        VariantType t -> fmap VariantType (rewriteType t)
        ArrayType t -> fmap ArrayType (rewriteType t)
        RecursiveType typ -> fmap RecursiveType (rewriteType typ)
        DeBruijnType i -> pure (DeBruijnType i)
        RowType TypeRow {..} -> do
          fields' <- traverse rewriteField fields
          typeVariable' <- traverse rewriteTypeVar typeVariable
          pure
            (RowType
               TypeRow {fields = fields', typeVariable = typeVariable', ..})
        ConstantType TypeConstant {..} -> pure (ConstantType TypeConstant {..})
        ApplyType TypeApplication {..} -> do
          function' <- rewriteType function
          argument' <- rewriteType argument
          pure
            (ApplyType
               TypeApplication {function = function', argument = argument', ..})
        VariableType typeVariable -> do
          generatedTypeVariable <- rewriteTypeVar typeVariable
          pure (VariableType generatedTypeVariable)
    rewriteField ::
         Field Polymorphic
      -> StateT (Map (TypeVariable Polymorphic) (TypeVariable Generated)) (Generate e) (Field Generated)
    rewriteField Field {..} = do
      typ' <- rewriteType typ
      pure Field {typ = typ', ..}
    rewriteTypeVar ::
         TypeVariable Polymorphic
      -> StateT (Map (TypeVariable Polymorphic) (TypeVariable Generated)) (Generate e) (TypeVariable Generated)
    rewriteTypeVar typeVariable@TypeVariable {..} = do
      scope <- get
      case M.lookup typeVariable scope of
        Just typeVariable' -> pure typeVariable'
        Nothing -> do
          generatedTypeVariable <-
            lift (generateTypeVariable location0 PolyPrefix kind)
          modify' (M.insert typeVariable generatedTypeVariable)
          pure generatedTypeVariable
