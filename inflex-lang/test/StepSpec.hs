{-# LANGUAGE OverloadedStrings #-}
-- |

module StepSpec where

import           Data.Text (Text)
import           Inflex.Display ()
import           Inflex.Stepper
import           RIO (textDisplay)
import           Test.Hspec

stepTextly :: Text -> Either (ResolveStepError ()) Text
stepTextly text = fmap textDisplay (stepText mempty mempty "" text)

stepDefaultedTextly :: Text -> Either (DefaultStepError ()) Text
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
    (do it "6" (shouldBe (stepDefaultedTextly "6") (Right "6"))
        it
          "-6"
          (do pendingWith "Need parser for negative numbers"
              shouldBe (stepDefaultedTextly "-6") (Right "-6"))
        it
          "-6.0"
          (do pendingWith "Need parser for negative numbers"
              shouldBe (stepDefaultedTextly "-6.0") (Right "-6.0"))
        it "6 + 3" (shouldBe (stepDefaultedTextly "6 + 3") (Right "9"))
        it "6 * 3" (shouldBe (stepDefaultedTextly "6 * 3") (Right "18"))
        it "6 / 3" (shouldBe (stepDefaultedTextly "6 / 3") (Right "2"))
        it "6 - 3" (shouldBe (stepDefaultedTextly "6 - 3") (Right "3"))
        it "6 - 3 * 3" (shouldBe (stepDefaultedTextly "6 - 3 * 3") (Right "-3"))
        it
          "6.2 + 3.10"
          (shouldBe (stepDefaultedTextly "6.2 + 3.10") (Right "9.30"))
        it
          "6.0 + 3.0"
          (shouldBe (stepDefaultedTextly "6.2 + 3.1") (Right "9.3"))
        it
          "6.0 - 3.0 * 3.0 / 2.0"
          (shouldBe (stepDefaultedTextly "6.0 - 3.0 * 3.0 / 2.0") (Right "1.5"))
        it
          "6.0 - 3 * 3.0 / 2.01"
          (shouldBe (stepDefaultedTextly "6.0 - 3 * 3.0 / 2.01") (Right "1.53"))
        it "6 + 3.10" (shouldBe (stepDefaultedTextly "6 + 3.10") (Right "9.10"))
        it
          "(2.0 :: Decimal 2)"
          (shouldBe (stepDefaultedTextly "(2.0 :: Decimal 2)") (Right "2.00"))
        it
          "(2 :: Decimal 3)"
          (shouldBe (stepDefaultedTextly "(2 :: Decimal 3)") (Right "2.000"))
        it
          "fromInteger (6 :: Integer) + 3.10 + fromDecimal (3.1 :: Decimal 1)"
          (shouldBe
             (stepDefaultedTextly
                "fromInteger (6 :: Integer) + 3.10 + fromDecimal (3.1 :: Decimal 1)")
             (Right "12.20")))
  describe
    "Sanity checks"
    (do it
          "0.1 + 0.1"
          (shouldBe (stepDefaultedTextly "0.1 + 0.2") (Right "0.3"))
        it "10/3" (shouldBe (stepDefaultedTextly "10/3") (Right "3"))
        it
          "5/2/2.000/2"
          (shouldBe (stepDefaultedTextly "5/2/2.000/2") (Right "0.625"))
        it
          "5/2/2.0/2"
          (shouldBe (stepDefaultedTextly "5/2/2.0/2") (Right "0.6"))
        it "1/3" (shouldBe (stepDefaultedTextly "1/3.000") (Right "0.333"))
        it "10/3.0" (shouldBe (stepDefaultedTextly "10/3.0") (Right "3.3"))
        it "10.0/0.0" pending
        it "10.0/0" pending)
