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
  ) where

import           Control.Monad.State
import           Data.Bifunctor
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Inflex.Instances ()
import           Inflex.Optics
import           Inflex.Renamer
import           Inflex.Types
import           Optics

--------------------------------------------------------------------------------
-- Types

data GenerateState = GenerateState
  { counter :: !Integer
  , classConstraints :: !(Seq (ClassConstraint Generated))
  } deriving (Show)

newtype Generate a = Generate
  { runGenerator :: State GenerateState a
  } deriving (Functor, Applicative, Monad)

data RenameGenerateError
  = RenameGenerateError ParseRenameError
  deriving (Show, Eq)

data HasConstraints a = HasConstraints
  { classes :: !(Seq (ClassConstraint Generated))
  , thing :: a
  } deriving (Show, Functor, Eq, Ord)

$(makeLensesWith (inflexRules ['counter, 'classConstraints]) ''GenerateState)

--------------------------------------------------------------------------------
-- Top-level

generateText :: FilePath -> Text -> Either RenameGenerateError (HasConstraints (Expression Generated))
generateText fp text = do
  expression <- first RenameGenerateError (renameText fp text)
  pure
    (let (expression', GenerateState {classConstraints = classes}) =
           runState
             (runGenerator (expressionGenerator expression))
             GenerateState {classConstraints = mempty, counter = 0}
      in HasConstraints {classes, thing = expression'})

--------------------------------------------------------------------------------
-- Generators

expressionGenerator :: Expression Renamed -> Generate (Expression Generated)
expressionGenerator =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (literalGenerator literal)

literalGenerator :: Literal Renamed -> Generate (Literal Generated)
literalGenerator =
  \case
    IntegerLiteral integery -> fmap IntegerLiteral (integeryGenerator integery)

integeryGenerator :: Integery Renamed -> Generate (Integery Generated)
integeryGenerator Integery {typ = _, ..} = do
  typ <- generateTypeVariable IntegeryPrefix
  addClassConstraint
    (ClassConstraint {className = FromInteger, types = pure typ})
  pure Integery {typ, ..}

--------------------------------------------------------------------------------
-- Type system helpers

generateTypeVariable :: TypeVariablePrefix -> Generate (Type Generated)
generateTypeVariable prefix =
  Generate
    (do i <- gets (view generateStateCounterL)
        modify' (over generateStateCounterL succ)
        pure (VariableType prefix i))

addClassConstraint :: ClassConstraint Generated -> Generate ()
addClassConstraint constraint =
  Generate (modify' (over generateStateClassConstraintsL (Seq.|> constraint)))
