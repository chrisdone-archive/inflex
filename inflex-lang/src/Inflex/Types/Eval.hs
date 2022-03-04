{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- |

module Inflex.Types.Eval where

import           Data.IORef
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import           GHC.Natural
import           Inflex.Defaulter
import           Inflex.Optics
import           Inflex.Types
import           Optics
import           RIO (GLogFunc, HasGLogFunc(..))

data DefaultEvalError e
  = DefaulterErrored (ResolverDefaulterError e)
  deriving (Show, Eq)

data Eval = Eval
  { glogfunc :: GLogFunc EvalMsg
  , globals :: Map Hash (Expression Resolved)
  , genericGlobalCache :: IORef (Map (Hash, NonEmpty InstanceName) (Expression Resolved))
  }

data EvalMsg
  = EvalStep (Expression Resolved)
  | GlobalMissing (Global Resolved)
  | CannotShrinkADecimalFromTo Natural Natural
  | MismatchingPrecisionsInFromDecimal Natural Natural
  | FoundGenericGlobalInCache Hash (NonEmpty InstanceName)
  | AddingGenericGlobalToCache Hash (NonEmpty InstanceName)
  | EncounteredGenericGlobal Hash (NonEmpty InstanceName)
  deriving (Show)

$(makeLensesWith (inflexRules ['glogfunc]) ''Eval)

instance HasGLogFunc Eval where
  type GMsg Eval = EvalMsg
  gLogFuncL = toLensVL evalGlogfuncL
