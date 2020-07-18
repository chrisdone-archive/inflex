{-# LANGUAGE OverloadedStrings #-}
-- |

module StepSpec where

import           Data.Text (Text)
import           Inflex.Display ()
import           Inflex.Stepper
import           RIO (textDisplay)
import           Test.Hspec

stepTextly :: Text -> Either ResolveStepError Text
stepTextly text = fmap textDisplay (stepText mempty mempty "" text)

spec :: SpecWith ()
spec =
  describe
    "Single expressions"
    (do it "6" (shouldBe (stepTextly "6 :: Integer") (Right "6"))
        it "6 + 3" (shouldBe (stepTextly "6 :: Integer + 3 :: Integer") (Right "9"))
        it "6 * 3" (shouldBe (stepTextly "6 :: Integer * 3 :: Integer") (Right "18"))
        it "6 / 3" (shouldBe (stepTextly "6 :: Integer / 3 :: Integer") (Right "2"))
        it "6 - 3" (shouldBe (stepTextly "6 :: Integer - 3 :: Integer") (Right "3"))
        it "6 - 3 * 3" (shouldBe (stepTextly "6 :: Integer - 3 :: Integer * 3 :: Integer") (Right "-3")))
