{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- |

module Inflex.Types.Eval where

import Data.Map.Strict (Map)
import GHC.Natural
import Inflex.Defaulter
import Inflex.Optics
import Inflex.Types
import Optics
import RIO (GLogFunc, HasGLogFunc(..))

data DefaultEvalError e
  = DefaulterErrored (ResolverDefaulterError e)
  deriving (Show, Eq)

data Eval = Eval
  { glogfunc :: GLogFunc EvalMsg
  , globals :: Map Hash (Expression Resolved)
  }

data EvalMsg
  = EvalStep (Expression Resolved)
  | GlobalMissing (Global Resolved)
  | CannotShrinkADecimalFromTo Natural Natural
  | MismatchingPrecisionsInFromDecimal Natural Natural
  deriving (Show)

$(makeLensesWith (inflexRules ['glogfunc]) ''Eval)

instance HasGLogFunc Eval where
  type GMsg Eval = EvalMsg
  gLogFuncL = toLensVL evalGlogfuncL
