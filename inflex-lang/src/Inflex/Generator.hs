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
  , RenameGenerateError(..)
  , HasConstraints(..)
  , hasConstraintsMappingsL
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Validate
import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Inflex.Instances ()
import           Inflex.Location
import           Inflex.Optics
import qualified Inflex.Renamer as Renamer
import           Inflex.Type
import           Inflex.Types
import           Numeric.Natural
import           Optics

--------------------------------------------------------------------------------
-- Types

data GenerateError =
  MissingVariableG (Variable Renamed)
  deriving (Show, Eq)

data GenerateState = GenerateState
  { counter :: !Natural
  , equalityConstraints :: !(Seq EqualityConstraint)
  } deriving (Show)

newtype Generate a = Generate
  { runGenerator :: ValidateT (NonEmpty GenerateError) (ReaderT Env (State GenerateState)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState GenerateState
             , MonadReader Env
             )

data Env = Env
  { scope :: ![Binding Generated]
  }

data RenameGenerateError
  = RenameGenerateError Renamer.ParseRenameError
  | GeneratorErrors (NonEmpty GenerateError)
  deriving (Show, Eq)

data HasConstraints a = HasConstraints
  { equalities :: !(Seq EqualityConstraint)
  , thing :: !a
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Functor, Eq, Ord)

$(makeLensesWith
    (inflexRules ['counter, 'equalityConstraints])
    ''GenerateState)
$(makeLensesWith (inflexRules ['scope]) ''Env)
$(makeLensesWith (inflexRules ['mappings]) ''HasConstraints)

--------------------------------------------------------------------------------
-- Top-level

generateText :: FilePath -> Text -> Either RenameGenerateError (HasConstraints (Expression Generated))
generateText fp text = do
  Renamer.IsRenamed {thing = expression, mappings} <-
    first RenameGenerateError (Renamer.renameText fp text)
  first
    GeneratorErrors
    (let (result, GenerateState { equalityConstraints
                                }) =
           runState
             (runReaderT
                (runValidateT (runGenerator (expressionGenerator expression)))
                (Env {scope = mempty}))
             GenerateState
               { counter = 0
               , equalityConstraints = mempty
               }
      in fmap
           (\thing ->
              HasConstraints
                {thing, mappings, equalities = equalityConstraints})
           result)

--------------------------------------------------------------------------------
-- Generators

expressionGenerator :: Expression Renamed -> Generate (Expression Generated)
expressionGenerator =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (literalGenerator literal)
    LambdaExpression lambda ->
      fmap LambdaExpression (lambdaGenerator lambda)
    ApplyExpression apply ->
      fmap ApplyExpression (applyGenerator apply)
    VariableExpression variable ->
      fmap VariableExpression (variableGenerator variable)
    GlobalExpression global ->
      fmap GlobalExpression (globalGenerator global)

literalGenerator :: Literal Renamed -> Generate (Literal Generated)
literalGenerator =
  \case
    NumberLiteral number -> fmap NumberLiteral (numberGenerator number)

numberGenerator :: Number Renamed -> Generate (Number Generated)
numberGenerator Number {typ = _, ..} =
  pure Number {typ = someNumberType location number, ..}

-- | Produce a immediately-known concrete type
someNumberType ::
     StagedLocation Renamed -> SomeNumber -> Type Generated
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

lambdaGenerator :: Lambda Renamed -> Generate (Lambda Generated)
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

paramGenerator :: Param Renamed -> Generate (Param Generated)
paramGenerator Param {typ = _, ..} = do
  typ <- generateTypeVariable location LambdaParameterPrefix TypeKind
  pure Param {typ, ..}

applyGenerator :: Apply Renamed -> Generate (Apply Generated)
applyGenerator Apply {..} = do
  function' <- expressionGenerator function
  argument' <- expressionGenerator argument
  outputType <-
    case typ of
      Nothing ->
        generateTypeVariable (expressionLocation argument') ApplyPrefix TypeKind
      Just typ' -> pure (toGenerated typ')
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

variableGenerator :: Variable Renamed -> Generate (Variable Generated)
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
      type1 <- generateTypeVariable location VariablePrefix TypeKind
      addEqualityConstraint EqualityConstraint {type1, type2, ..}
      pure Variable {typ = type1, name = index, ..}

globalGenerator :: Global Renamed -> Generate (Global Generated)
globalGenerator Global {name, location} = do
  scheme <-
    case name of
      FromIntegerGlobal -> do
        typeVariable <- generateTypeVariable location IntegerPrefix TypeKind
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
        numberVar <- generateTypeVariable location DecimalPrefix TypeKind
        precisionVar <- generateTypeVariable location NatPrefix NatKind
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

--------------------------------------------------------------------------------
-- Type system helpers

generateTypeVariable ::
     StagedLocation Generated
  -> TypeVariablePrefix
  -> Kind
  -> Generate (Type Generated)
generateTypeVariable location prefix kind = do
  index <- gets (view generateStateCounterL)
  modify' (over generateStateCounterL succ)
  pure (VariableType TypeVariable {prefix, index, location, kind})

addEqualityConstraint :: EqualityConstraint -> Generate ()
addEqualityConstraint constraint =
  modify' (over generateStateEqualityConstraintsL (Seq.|> constraint))

--------------------------------------------------------------------------------
-- Generation of renamed type

toGenerated :: Type Renamed -> Type Generated
toGenerated =
  \case
    VariableType TypeVariable {..} -> VariableType TypeVariable {..}
    ConstantType TypeConstant {..} -> ConstantType TypeConstant {..}
    ApplyType TypeApplication {..} ->
      ApplyType
        TypeApplication
          {function = toGenerated function, argument = toGenerated argument, ..}
