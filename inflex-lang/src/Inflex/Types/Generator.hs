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
import           Inflex.Types.Filler
import           Numeric.Natural
import           Optics

--------------------------------------------------------------------------------
-- Types

data GenerateError e
  = MissingVariableG (Variable Filled)
  | MissingHashG Hash
  | OtherCellErrorG (GlobalRef Renamed) e
  deriving (Show, Eq)

data GenerateState = GenerateState
  { counter :: !Natural
  , equalityConstraints :: !(Seq EqualityConstraint)
  } deriving (Show)

newtype Generate e a = Generate
  { runGenerator :: ValidateT (NonEmpty (GenerateError e)) (ReaderT (Env e) (State GenerateState)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState GenerateState
             , MonadReader (Env e)
             )

data Env e = Env
  { scope :: ![Binding Generated]
  , globals :: Map Hash (Either e (Scheme Polymorphic))
  }

data RenameGenerateError e
  = RenameGenerateError Renamer.ParseRenameError
  | GeneratorErrors (NonEmpty (GenerateError e))
  | FillErrors (NonEmpty (FillerError e))
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
$(makeLensesWith (inflexRules ['Inflex.Types.Generator.mappings]) ''HasConstraints)
