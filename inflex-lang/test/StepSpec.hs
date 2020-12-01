{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module StepSpec where

import           Data.Fixed
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Display ()
import           Inflex.Stepper
import           RIO (textDisplay)
import           Test.Hspec
import           Test.QuickCheck

stepTextly :: Text -> Either (ResolveStepError ()) Text
stepTextly text = fmap textDisplay (stepText mempty mempty "" text)

stepDefaultedTextly :: Text -> Either (DefaultStepError ()) Text
stepDefaultedTextly text = fmap textDisplay (stepTextDefaulted mempty mempty "" text)

spec :: SpecWith ()
spec = do
  describe
    "Single expressions"
    (do it "6" (shouldBe (stepTextly "6 :: Integer") (Right "6"))
        it "6 + _" (shouldBe (stepDefaultedTextly "(6 + _)") (Right "(6 + _)"))
        it
          "2 * 6 + _"
          (shouldBe (stepDefaultedTextly "(2 * 6 + _)") (Right "(12 + _)"))
        it
          "2 * 3 + _ / 2 + 3"
          (shouldBe
             (stepDefaultedTextly "2 * 3 + _ / 2 + 3")
             (Right "((6 + (_ / 2)) + 3)"))
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
        it "-6" (shouldBe (stepDefaultedTextly "-6") (Right "-6"))
        it "-6.0" (shouldBe (stepDefaultedTextly "-6.0") (Right "-6.0"))
        it "-6.0 * 2" (shouldBe (stepDefaultedTextly "-6.0*2") (Right "-12.0"))
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
  describe
    "Variants"
    (do it "#true" (shouldBe (stepDefaultedTextly "#true") (Right "#true"))
        it
          "#ok(x*y)"
          (property
             (\(x :: Integer) y ->
                shouldBe
                  (stepDefaultedTextly
                     (T.pack ("#ok(" <> show x <> "*" <> show y <> ")")))
                  (Right (T.pack ("#ok(" <> show (x * y) <> ")"))))))
  equality
  ordering
  functions

equality :: SpecWith ()
equality =
  describe
    "Equality"
    (do describe
          "Text"
          (do it
                "\"x\"=\"x\""
                (shouldBe (stepDefaultedTextly "\"x\"=\"x\"") (Right "#true"))
              it
                "\"x\"=\"y\""
                (shouldBe (stepDefaultedTextly "\"x\"=\"y\"") (Right "#false"))
              it
                "\"x\"/=\"y\""
                (shouldBe (stepDefaultedTextly "\"x\"/=\"y\"") (Right "#true"))
              it
                "\"\"=\"\""
                (shouldBe (stepDefaultedTextly "\"\"=\"\"") (Right "#true"))
              it
                "\"\"/=\"\""
                (shouldBe (stepDefaultedTextly "\"\"/=\"\"") (Right "#false")))
        describe
          "Integer"
          (do it "1=1" (shouldBe (stepDefaultedTextly "1=1") (Right "#true"))
              it "1=2" (shouldBe (stepDefaultedTextly "1=2") (Right "#false"))
              it "1/=2" (shouldBe (stepDefaultedTextly "1/=2") (Right "#true"))
              it
                "2*3=6"
                (shouldBe (stepDefaultedTextly "2*3=6") (Right "#true"))
              it
                "n=y (integer)"
                (property
                   (\(x :: Integer) y ->
                      shouldBe
                        (stepDefaultedTextly (T.pack (show x ++ "=" ++ show y)))
                        (Right (quoteBool (x == y))))))
        describe
          "Decimal"
          (do it
                "1.0=1.0"
                (shouldBe (stepDefaultedTextly "1.0=1.0") (Right "#true"))
              it
                "1.0=1"
                (shouldBe (stepDefaultedTextly "1.0=1") (Right "#true"))
              it
                "1.0=2"
                (shouldBe (stepDefaultedTextly "1.0=2") (Right "#false"))
              it
                "1.0/=2"
                (shouldBe (stepDefaultedTextly "1.0/=2") (Right "#true"))
              it
                "1.0=1.00"
                (shouldBe (stepDefaultedTextly "1.0=1.00") (Right "#true"))
              it
                "1.0*6=3.00*2"
                (shouldBe (stepDefaultedTextly "1.0*6=3.00*2") (Right "#true"))
              it
                "1.0*6=3.00*3"
                (shouldBe (stepDefaultedTextly "1.0*6=3.00*3") (Right "#false"))
              it
                "n=y (decimal 2)"
                (property
                   (\(x :: Centi) y ->
                      shouldBe
                        (stepDefaultedTextly (T.pack (show x ++ "=" ++ show y)))
                        (Right (quoteBool (x == y)))))))

ordering :: SpecWith ()
ordering =
  describe
    "Ordering"
    (do describe
          "Numbers"
          (do it
                "1.0>1.0"
                (shouldBe (stepDefaultedTextly "1.0>1.0") (Right "#false"))
              it
                "1.0>0.0"
                (shouldBe (stepDefaultedTextly "1.0>0.0") (Right "#true"))
              it "1>1" (shouldBe (stepDefaultedTextly "1>1") (Right "#false"))
              it "1>0" (shouldBe (stepDefaultedTextly "1>0") (Right "#true"))
              it "1<0" (shouldBe (stepDefaultedTextly "1<0") (Right "#false"))
              it "1<=0" (shouldBe (stepDefaultedTextly "1<=0") (Right "#false"))
              it "0<=1" (shouldBe (stepDefaultedTextly "0<=1") (Right "#true"))
              it "0<=0" (shouldBe (stepDefaultedTextly "0<=0") (Right "#true"))
              it "1>=0" (shouldBe (stepDefaultedTextly "1>=0") (Right "#true"))
              it "0>=1" (shouldBe (stepDefaultedTextly "0>=1") (Right "#false"))
              it "0>=0" (shouldBe (stepDefaultedTextly "0>=0") (Right "#true")))
        describe
          "Text"
          (do it
                "\"x\"<\"y\""
                (shouldBe (stepDefaultedTextly "\"x\"<\"y\"") (Right "#true"))
              it
                "\"abc\">\"abb\""
                (shouldBe
                   (stepDefaultedTextly "\"abc\">\"abb\"")
                   (Right "#true"))
              it
                "\"x\"<=\"x\""
                (shouldBe (stepDefaultedTextly "\"x\"<=\"x\"") (Right "#true"))
              it
                "\"x\">\"x\""
                (shouldBe (stepDefaultedTextly "\"x\">\"x\"") (Right "#false"))
              it
                "\"x\">\"y\""
                (shouldBe (stepDefaultedTextly "\"x\">\"y\"") (Right "#false"))))

quoteBool :: Bool -> Text
quoteBool True = "#true"
quoteBool False = "#false"

functions :: SpecWith ()
functions =
  describe
    "Functions"
    (do describe
          "map"
          (do it
                "map(r:r*2,[1,2,3])"
                (shouldBe
                   (stepDefaultedTextly "map(r:r*2,[1,2,3])")
                   (Right "[2, 4, 6]"))
              it
                "map(r:r.x*2,[{x:1},{x:2},{x:3}])"
                (shouldBe
                   (stepDefaultedTextly "map(r:r.x*2,[{x:1},{x:2},{x:3}])")
                   (Right "[2, 4, 6]")))
        describe
          "filter"
          (do it
                "filter(r:r.x>=2, [{x:1},{x:2},{x:3}])"
                (shouldBe
                   (stepDefaultedTextly "filter(r:r.x>=2, [{x:1},{x:2},{x:3}])")
                   (Right "[{x: 2}, {x: 3}]")))
        describe
          "length"
          (do it
                "length([1,2,3])"
                (shouldBe
                   (stepDefaultedTextly "length([1,2,3])")
                   (Right "3"))
              it
                "null([1,2,3])"
                (shouldBe
                   (stepDefaultedTextly "null([1,2,3])")
                   (Right "#false"))))
