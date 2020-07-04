{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Type generator for Inflex.

module Inflex.Types.Generator
   where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Validate
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq)
import           Inflex.Instances ()
import           Inflex.Optics
import qualified Inflex.Renamer as Renamer
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
