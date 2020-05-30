{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module SolverSpec where

import           Inflex.Instances ()
import           Inflex.Solver
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe
    "Unit tests"
    (do it "a ~ a" (shouldBe (solveConstraints [a .~. a]) [])
        it
          "a ~ b"
          (do pending
              shouldBe (solveConstraints [a .~. b]) [a' .-> b]))
  where
    (.~.) x y =
      EqualityConstraint {location = ExpressionCursor, type1 = x, type2 = y}
    (.->) x y = Substitution {before = x, after = y}
    a' =
      TypeVariable
        {location = ExpressionCursor, prefix = IntegeryPrefix, index = 0}
    a = VariableType a'
    b =
      VariableType
        TypeVariable
          {location = ExpressionCursor, prefix = IntegeryPrefix, index = 1}
