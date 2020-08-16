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
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Validate
import           Data.Bifunctor
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Data.Validation
import           Inflex.Filler
import           Inflex.Instances ()
import           Inflex.Location
import qualified Inflex.Renamer as Renamer
import           Inflex.Type
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import           Optics

--------------------------------------------------------------------------------
-- Top-level

generateText ::
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (RenameGenerateError e) (HasConstraints (Expression Generated))
generateText globals fp text = do
  isrenamed <- first RenameGenerateError (Renamer.renameText fp text)
  generateRenamed globals mempty isrenamed

generateRenamed ::
     Map Hash (Either e (Scheme Polymorphic))
  -> Map Text (Either e Hash)
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
    LambdaExpression lambda ->
      fmap LambdaExpression (lambdaGenerator lambda)
    LetExpression let' ->
      fmap LetExpression (letGenerator let')
    InfixExpression infix' ->
      fmap InfixExpression (infixGenerator infix')
    ApplyExpression apply ->
      fmap ApplyExpression (applyGenerator apply)
    VariableExpression variable ->
      fmap VariableExpression (variableGenerator variable)
    GlobalExpression global ->
      fmap GlobalExpression (globalGenerator global)

literalGenerator :: Literal Filled -> Generate e (Literal Generated)
literalGenerator =
  \case
    NumberLiteral number -> fmap NumberLiteral (numberGenerator number)

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
lambdaGenerator Lambda {typ = _, ..} = do
  param' <- paramGenerator param
  body' <- local (over envScopeL (LambdaBinding param' :)) (expressionGenerator body)
  let outputType = expressionType body'
  pure
    Lambda
      { typ =
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
      , body = body'
      , param = param'
      , ..
      }

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
    EqualityConstraint {type1 = expressionType left', type2 = ty, ..}
  addEqualityConstraint
    EqualityConstraint {type1 = expressionType right', type2 = ty, ..}
  addEqualityConstraint
    EqualityConstraint
      {type1 = globalType global', type2 = funcType location ty ty, ..}
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
      Just typ' -> pure (renamedToGenerated typ')
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
            LetBinding params
              | DeBrujinIndexOfLet _ (IndexInLet subIndex) <- index ->
                lookup subIndex (zip [0..] (toList params))
            _ -> Nothing of
    Nothing -> Generate (refute (pure (MissingVariableG variable)))
    Just Param {typ = type2} -> do
      type1 <- generateVariableType location VariablePrefix TypeKind
      addEqualityConstraint EqualityConstraint {type1, type2, ..}
      pure Variable {typ = type1, name = index, ..}

globalGenerator :: Global Filled -> Generate e (Global Generated)
globalGenerator Global {name, location} = do
  scheme <-
    case name of
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
            , typ = funcType location a a
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
            , typ =
                ApplyType
                  TypeApplication
                    { function =
                        ApplyType
                          TypeApplication
                            { function =
                                ConstantType
                                  TypeConstant
                                    {name = FunctionTypeName, location}
                            , argument =
                                ConstantType
                                  TypeConstant
                                    {name = IntegerTypeName, location}
                            , kind = FunKind TypeKind TypeKind
                            , ..
                            }
                    , argument = typeVariable
                    , kind = TypeKind
                    , ..
                    }
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
            , typ =
                ApplyType
                  TypeApplication
                    { function =
                        ApplyType
                          TypeApplication
                            { function =
                                ConstantType
                                  TypeConstant
                                    {name = FunctionTypeName, location}
                            , argument =
                                ApplyType
                                  TypeApplication
                                    { function =
                                        ConstantType
                                          TypeConstant
                                            {name = DecimalTypeName, location}
                                    , argument = precisionVar
                                    , kind = TypeKind
                                    , ..
                                    }
                            , kind = FunKind TypeKind TypeKind
                            , ..
                            }
                    , argument = numberVar
                    , kind = TypeKind
                    , ..
                    }
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
     StagedLocation Generated
  -> TypeVariablePrefix
  -> Kind
  -> Generate e (TypeVariable Generated)
generateTypeVariable location prefix kind = do
  index <- gets (view generateStateCounterL)
  modify' (over generateStateCounterL succ)
  pure (TypeVariable {prefix, index, location, kind})

generateVariableType ::
     StagedLocation Generated
  -> TypeVariablePrefix
  -> Kind
  -> Generate e (Type Generated)
generateVariableType location prefix kind =
  fmap VariableType (generateTypeVariable location prefix kind)

addEqualityConstraint :: EqualityConstraint -> Generate e ()
addEqualityConstraint constraint =
  modify' (over generateStateEqualityConstraintsL (Seq.|> constraint))

funcType :: StagedLocation s -> Type s -> Type s -> Type s
funcType location inp out =
  ApplyType
    TypeApplication
      { function =
          ApplyType
            TypeApplication
              { function =
                  ConstantType TypeConstant {name = FunctionTypeName, ..}
              , argument = inp
              , kind = TypeKind
              , ..
              }
      , argument = out
      , kind = TypeKind
      , ..
      }

--------------------------------------------------------------------------------
-- Generation of renamed type

renamedToGenerated :: Type Renamed -> Type Generated
renamedToGenerated =
  \case
    VariableType TypeVariable {..} -> VariableType TypeVariable {..}
    ConstantType TypeConstant {..} -> ConstantType TypeConstant {..}
    ApplyType TypeApplication {..} ->
      ApplyType
        TypeApplication
          {function = renamedToGenerated function, argument = renamedToGenerated argument, ..}

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
        ConstantType TypeConstant {..} -> pure (ConstantType TypeConstant {..})
        ApplyType TypeApplication {..} -> do
          function' <- rewriteType function
          argument' <- rewriteType argument
          pure
            (ApplyType
               TypeApplication {function = function', argument = argument', ..})
        VariableType typeVariable@TypeVariable {..} -> do
          scope <- get
          case M.lookup typeVariable scope of
            Just typeVariable' -> pure (VariableType typeVariable')
            Nothing -> do
              generatedTypeVariable <-
                lift (generateTypeVariable location0 PolyPrefix kind)
              modify (M.insert typeVariable generatedTypeVariable)
              pure (VariableType generatedTypeVariable)
