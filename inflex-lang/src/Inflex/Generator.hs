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
  , classConstraints :: !(Seq (ClassConstraint Generated))
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
  { scope :: ![Param Generated]
  }

data RenameGenerateError
  = RenameGenerateError Renamer.ParseRenameError
  | GeneratorErrors (NonEmpty GenerateError)
  deriving (Show, Eq)

data HasConstraints a = HasConstraints
  { classes :: !(Seq (ClassConstraint Generated))
  , equalities :: !(Seq EqualityConstraint)
  , thing :: !a
  , mappings :: !(Map Cursor SourceLocation)
  } deriving (Show, Functor, Eq, Ord)

$(makeLensesWith
    (inflexRules ['counter, 'classConstraints, 'equalityConstraints])
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
    (let (result, GenerateState { classConstraints = classes
                                , equalityConstraints
                                }) =
           runState
             (runReaderT
                (runValidateT (runGenerator (expressionGenerator expression)))
                (Env {scope = mempty}))
             GenerateState
               { classConstraints = mempty
               , counter = 0
               , equalityConstraints = mempty
               }
      in fmap
           (\thing ->
              HasConstraints
                {classes, thing, mappings, equalities = equalityConstraints})
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

literalGenerator :: Literal Renamed -> Generate (Literal Generated)
literalGenerator =
  \case
    IntegerLiteral integery -> fmap IntegerLiteral (integeryGenerator integery)

integeryGenerator :: Integery Renamed -> Generate (Integery Generated)
integeryGenerator Integery {typ = _, ..} = do
  typ <- generateTypeVariable location IntegeryPrefix TypeKind
  addClassConstraint
    (ClassConstraint
       {className = FromIntegerClassName, types = pure typ, location})
  pure Integery {typ, ..}

lambdaGenerator :: Lambda Renamed -> Generate (Lambda Generated)
lambdaGenerator Lambda {typ = _, ..} = do
  param' <- paramGenerator param
  body' <- local (over envScopeL (param' :)) (expressionGenerator body)
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
applyGenerator Apply {typ = _, ..} = do
  function' <- expressionGenerator function
  argument' <- expressionGenerator argument
  outputType <- generateTypeVariable (expressionLocation argument') ApplyPrefix TypeKind
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
variableGenerator variable@Variable { typ = _
                                    , name = name@(DeBrujinIndex index)
                                    , ..
                                    } = do
  Env {scope} <- ask
  case lookup index (zip [0 ..] scope) of
    Nothing -> Generate (refute (pure (MissingVariableG variable)))
    Just Param {typ = type2} -> do
      type1 <- generateTypeVariable location VariablePrefix TypeKind
      addEqualityConstraint
        EqualityConstraint {type1, type2, ..}
      pure Variable {typ = type1, name, ..}

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

addClassConstraint :: ClassConstraint Generated -> Generate ()
addClassConstraint constraint =
  modify' (over generateStateClassConstraintsL (Seq.|> constraint))

addEqualityConstraint :: EqualityConstraint -> Generate ()
addEqualityConstraint constraint =
  modify' (over generateStateEqualityConstraintsL (Seq.|> constraint))
