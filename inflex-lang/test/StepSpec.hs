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

stepDefaultedTextly :: Text -> Either DefaultStepError Text
stepDefaultedTextly text = fmap textDisplay (stepTextDefaulted mempty mempty "" text)

spec :: SpecWith ()
spec = do
  describe
    "Single expressions"
    (do it "6" (shouldBe (stepTextly "6 :: Integer") (Right "6"))
        it
          "6 + 3"
          (shouldBe (stepTextly "6 :: Integer + 3 :: Integer") (Right "9"))
        it
          "6 * 3"
          (shouldBe (stepTextly "6 :: Integer * 3 :: Integer") (Right "18"))
        it
          "6 / 3"
          (shouldBe (stepTextly "6 :: Integer / 3 :: Integer") (Right "2"))
        it
          "6 - 3"
          (shouldBe (stepTextly "6 :: Integer - 3 :: Integer") (Right "3"))
        it
          "6 - 3 * 3"
          (shouldBe
             (stepTextly "6 :: Integer - 3 :: Integer * 3 :: Integer")
             (Right "-3"))
        it
          "6.0 + 3.0"
          (shouldBe
             (stepTextly "6.20 :: Decimal 2 + 3.10 :: Decimal 2")
             (Right "9.30"))
        it
          "6.0 - 3.0 * 3.0 / 2.0"
          (shouldBe
             (stepTextly
                "6.00 :: Decimal 2 - 3.00 :: Decimal 2 * 3.00 :: Decimal 2 / 2.00 :: Decimal 2")
             (Right "1.50"))
        it "6 + 3" (shouldBe (stepDefaultedTextly "6 + 3") (Right "9"))
        it
          "6 + 3 :: Integer"
          (shouldBe (stepDefaultedTextly "6 + 3 :: Integer") (Right "9")))
  describe
    "Defaulted single expressions"
    (do it
          "6"
          (do
              shouldBe (stepDefaultedTextly "6") (Right "6"))
        it
          "6 + 3"
          (do
              shouldBe (stepDefaultedTextly "6 + 3") (Right "9"))
        it
          "6 * 3"
          (do
              shouldBe (stepDefaultedTextly "6 * 3") (Right "18"))
        it
          "6 / 3"
          (do
              shouldBe (stepDefaultedTextly "6 / 3") (Right "2"))
        it
          "6 - 3"
          (do
              shouldBe (stepDefaultedTextly "6 - 3") (Right "3"))
        it
          "6 - 3 * 3"
          (do
              shouldBe (stepDefaultedTextly "6 - 3 * 3") (Right "-3"))
        it
          "6.2 + 3.10"
          (do
              shouldBe (stepDefaultedTextly "6.2 + 3.10") (Right "9.30"))
        it
          "6.0 + 3.0"
          (do
              shouldBe (stepDefaultedTextly "6.2 + 3.1") (Right "9.3"))
        it
          "6.0 - 3.0 * 3.0 / 2.0"
          (do
              shouldBe (stepDefaultedTextly "6.0 - 3.0 * 3.0 / 2.0") (Right "1.5"))
        it
          "6 + 3.10"
          (do
              shouldBe (stepDefaultedTextly "6 + 3.10") (Right "9.10")))
