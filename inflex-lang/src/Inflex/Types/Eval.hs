{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- |

module Inflex.Types.Eval where

import           Data.IORef
import           Data.Map.Strict (Map)
import           Data.Set (Set)
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
  , genericGlobalCache :: IORef (Map (Hash, Set InstanceName) (Expression Resolved))
  }

data EvalMsg
  = EvalStep (Expression Resolved)
  | GlobalMissing (Global Resolved)
  | CannotShrinkADecimalFromTo Natural Natural
  | MismatchingPrecisionsInFromDecimal Natural Natural
  | FoundGenericGlobalInCache Hash (Set InstanceName)
  | FoundMonoGlobalInCache Hash (Set InstanceName)
  | AddingGenericGlobalToCache Hash (Set InstanceName)
  | AddingGenericGlobalToCacheFromCell1 Hash (Set InstanceName)
  | EncounteredGenericGlobal Hash (Set InstanceName)
  deriving (Show)

$(makeLensesWith (inflexRules ['glogfunc]) ''Eval)

instance HasGLogFunc Eval where
  type GMsg Eval = EvalMsg
  gLogFuncL = toLensVL evalGlogfuncL
