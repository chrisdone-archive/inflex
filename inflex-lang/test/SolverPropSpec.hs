{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Test the solver with GHC as reference implementation.

module SolverPropSpec where

import Language.Haskell.Interpreter
import Test.Hspec

--------------------------------------------------------------------------------
-- Test suite

spec :: Spec
spec =
  it
    "Hint"
    (shouldReturn
       (runInterpreter
          (do set [languageExtensions := [ExplicitForAll, GADTs]]
              setImportsQ [("Prelude", Nothing)]
              typeChecksWithDetails
                "let f :: forall t a b. (t ~ (->) a a, (->) a a ~ (->) (Maybe b) (Maybe Integer)) => t; f = undefined in f"))
       (Right (Right "Maybe Integer -> Maybe Integer")))

--------------------------------------------------------------------------------
-- Orphans

deriving instance Eq InterpreterError
deriving instance Eq GhcError
