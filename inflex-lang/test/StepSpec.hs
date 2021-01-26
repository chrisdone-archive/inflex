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
import qualified RIO
import           RIO (textDisplay)
import           Test.Hspec
import           Test.QuickCheck

stepTextly :: Text -> IO (Either (ResolveStepError ()) Text)
stepTextly text = RIO.runRIO StepReader (fmap (fmap textDisplay) (stepText mempty mempty "" text))

stepDefaultedTextly :: Text -> IO (Either (DefaultStepError ()) Text)
stepDefaultedTextly text =
  RIO.runRIO StepReader (fmap (fmap textDisplay) (stepTextDefaulted mempty mempty "" text))

spec :: SpecWith ()
spec = do
  describe
    "Single expressions"
    (do it "6" (shouldReturn (stepTextly "6 :: Integer") (Right "6"))
        it "6 + _" (shouldReturn (stepDefaultedTextly "(6 + _)") (Right "(6 + _)"))
        it
          "2 * 6 + _"
          (shouldReturn (stepDefaultedTextly "(2 * 6 + _)") (Right "(12 + _)"))
        it
          "2 * 3 + _ / 2 + 3"
          (shouldReturn
             (stepDefaultedTextly "2 * 3 + _ / 2 + 3")
             (Right "((6 + (_ / 2)) + 3)"))
        it
          "6 + 3"
          (shouldReturn (stepTextly "6 :: Integer + 3 :: Integer") (Right "9"))
        it
          "6 * 3"
          (shouldReturn (stepTextly "6 :: Integer * 3 :: Integer") (Right "18"))
        it
          "6 / 3"
          (shouldReturn (stepTextly "6 :: Integer / 3 :: Integer") (Right "2"))
        it
          "6 - 3"
          (shouldReturn (stepTextly "6 :: Integer - 3 :: Integer") (Right "3"))
        it
          "6 - 3 * 3"
          (shouldReturn
             (stepTextly "6 :: Integer - 3 :: Integer * 3 :: Integer")
             (Right "-3"))
        it
          "6.0 + 3.0"
          (shouldReturn
             (stepTextly "6.20 :: Decimal 2 + 3.10 :: Decimal 2")
             (Right "9.30"))
        it
          "6.0 - 3.0 * 3.0 / 2.0"
          (shouldReturn
             (stepTextly
                "6.00 :: Decimal 2 - 3.00 :: Decimal 2 * 3.00 :: Decimal 2 / 2.00 :: Decimal 2")
             (Right "1.50"))
        it "6 + 3" (shouldReturn (stepDefaultedTextly "6 + 3") (Right "9"))
        it
          "6 + 3 :: Integer"
          (shouldReturn (stepDefaultedTextly "6 + 3 :: Integer") (Right "9")))
  describe
    "Defaulted single expressions"
    (do it "6" (shouldReturn (stepDefaultedTextly "6") (Right "6"))
        it "-6" (shouldReturn (stepDefaultedTextly "-6") (Right "-6"))
        it "-6.0" (shouldReturn (stepDefaultedTextly "-6.0") (Right "-6.0"))
        it "-6.0 * 2" (shouldReturn (stepDefaultedTextly "-6.0*2") (Right "-12.0"))
        it "6 + 3" (shouldReturn (stepDefaultedTextly "6 + 3") (Right "9"))
        it "6 * 3" (shouldReturn (stepDefaultedTextly "6 * 3") (Right "18"))
        it "6 / 3" (shouldReturn (stepDefaultedTextly "6 / 3") (Right "2"))
        it "6 - 3" (shouldReturn (stepDefaultedTextly "6 - 3") (Right "3"))
        it "6 - 3 * 3" (shouldReturn (stepDefaultedTextly "6 - 3 * 3") (Right "-3"))
        it
          "6.2 + 3.10"
          (shouldReturn (stepDefaultedTextly "6.2 + 3.10") (Right "9.30"))
        it
          "6.0 + 3.0"
          (shouldReturn (stepDefaultedTextly "6.2 + 3.1") (Right "9.3"))
        it
          "6.0 - 3.0 * 3.0 / 2.0"
          (shouldReturn (stepDefaultedTextly "6.0 - 3.0 * 3.0 / 2.0") (Right "1.5"))
        it
          "6.0 - 3 * 3.0 / 2.01"
          (shouldReturn (stepDefaultedTextly "6.0 - 3 * 3.0 / 2.01") (Right "1.53"))
        it "6 + 3.10" (shouldReturn (stepDefaultedTextly "6 + 3.10") (Right "9.10"))
        it
          "(2.0 :: Decimal 2)"
          (shouldReturn (stepDefaultedTextly "(2.0 :: Decimal 2)") (Right "2.00"))
        it
          "(2 :: Decimal 3)"
          (shouldReturn (stepDefaultedTextly "(2 :: Decimal 3)") (Right "2.000"))
        it
          "fromInteger (6 :: Integer) + 3.10 + fromDecimal (3.1 :: Decimal 1)"
          (shouldReturn
             (stepDefaultedTextly
                "fromInteger (6 :: Integer) + 3.10 + fromDecimal (3.1 :: Decimal 1)")
             (Right "12.20")))
  describe
    "Sanity checks"
    (do it
          "0.1 + 0.1"
          (shouldReturn (stepDefaultedTextly "0.1 + 0.2") (Right "0.3"))
        it "10/3" (shouldReturn (stepDefaultedTextly "10/3") (Right "3"))
        it
          "5/2/2.000/2"
          (shouldReturn (stepDefaultedTextly "5/2/2.000/2") (Right "0.625"))
        it
          "5/2/2.0/2"
          (shouldReturn (stepDefaultedTextly "5/2/2.0/2") (Right "0.6"))
        it "1/3" (shouldReturn (stepDefaultedTextly "1/3.000") (Right "0.333"))
        it "10/3.0" (shouldReturn (stepDefaultedTextly "10/3.0") (Right "3.3"))
        it "10.0/0.0" (shouldReturn (stepDefaultedTextly "10.0/0.0") (Right "(10.0 / 0.0)"))
        it "10/0" (shouldReturn (stepDefaultedTextly "10/0") (Right "(10 / 0)"))
        it "10.0/0" (shouldReturn (stepDefaultedTextly "10.0/0") (Right "(10.0 / 0.0)")))
  describe
    "Variants"
    (do it "#true" (shouldReturn (stepDefaultedTextly "#true") (Right "#true"))
        it
          "#ok(x*y)"
          (property
             (\(x :: Integer) y ->
                shouldReturn
                  (stepDefaultedTextly
                     (T.pack ("#ok(" <> show x <> "*" <> show y <> ")")))
                  (Right (T.pack ("#ok(" <> show (x * y) <> ")"))))))
  equality
  ordering
  functions
  if'
  case'
  early

equality :: SpecWith ()
equality =
  describe
    "Equality"
    (do describe
          "Text"
          (do it
                "\"x\"=\"x\""
                (shouldReturn (stepDefaultedTextly "\"x\"=\"x\"") (Right "#true"))
              it
                "\"x\"=\"y\""
                (shouldReturn (stepDefaultedTextly "\"x\"=\"y\"") (Right "#false"))
              it
                "\"x\"/=\"y\""
                (shouldReturn (stepDefaultedTextly "\"x\"/=\"y\"") (Right "#true"))
              it
                "\"\"=\"\""
                (shouldReturn (stepDefaultedTextly "\"\"=\"\"") (Right "#true"))
              it
                "\"\"/=\"\""
                (shouldReturn (stepDefaultedTextly "\"\"/=\"\"") (Right "#false")))
        describe
          "Integer"
          (do it "1=1" (shouldReturn (stepDefaultedTextly "1=1") (Right "#true"))
              it "1=2" (shouldReturn (stepDefaultedTextly "1=2") (Right "#false"))
              it "1/=2" (shouldReturn (stepDefaultedTextly "1/=2") (Right "#true"))
              it
                "2*3=6"
                (shouldReturn (stepDefaultedTextly "2*3=6") (Right "#true"))
              it
                "n=y (integer)"
                (property
                   (\(x :: Integer) y ->
                      shouldReturn
                        (stepDefaultedTextly (T.pack (show x ++ "=" ++ show y)))
                        (Right (quoteBool (x == y))))))
        describe
          "Decimal"
          (do it
                "1.0=1.0"
                (shouldReturn (stepDefaultedTextly "1.0=1.0") (Right "#true"))
              it
                "1.0=1"
                (shouldReturn (stepDefaultedTextly "1.0=1") (Right "#true"))
              it
                "1.0=2"
                (shouldReturn (stepDefaultedTextly "1.0=2") (Right "#false"))
              it
                "1.0/=2"
                (shouldReturn (stepDefaultedTextly "1.0/=2") (Right "#true"))
              it
                "1.0=1.00"
                (shouldReturn (stepDefaultedTextly "1.0=1.00") (Right "#true"))
              it
                "1.0*6=3.00*2"
                (shouldReturn (stepDefaultedTextly "1.0*6=3.00*2") (Right "#true"))
              it
                "1.0*6=3.00*3"
                (shouldReturn (stepDefaultedTextly "1.0*6=3.00*3") (Right "#false"))
              it
                "n=y (decimal 2)"
                (property
                   (\(x :: Centi) y ->
                      shouldReturn
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
                (shouldReturn (stepDefaultedTextly "1.0>1.0") (Right "#false"))
              it
                "1.0>0.0"
                (shouldReturn (stepDefaultedTextly "1.0>0.0") (Right "#true"))
              it "1>1" (shouldReturn (stepDefaultedTextly "1>1") (Right "#false"))
              it "1>0" (shouldReturn (stepDefaultedTextly "1>0") (Right "#true"))
              it "1<0" (shouldReturn (stepDefaultedTextly "1<0") (Right "#false"))
              it "1<=0" (shouldReturn (stepDefaultedTextly "1<=0") (Right "#false"))
              it "0<=1" (shouldReturn (stepDefaultedTextly "0<=1") (Right "#true"))
              it "0<=0" (shouldReturn (stepDefaultedTextly "0<=0") (Right "#true"))
              it "1>=0" (shouldReturn (stepDefaultedTextly "1>=0") (Right "#true"))
              it "0>=1" (shouldReturn (stepDefaultedTextly "0>=1") (Right "#false"))
              it "0>=0" (shouldReturn (stepDefaultedTextly "0>=0") (Right "#true")))
        describe
          "Text"
          (do it
                "\"x\"<\"y\""
                (shouldReturn (stepDefaultedTextly "\"x\"<\"y\"") (Right "#true"))
              it
                "\"abc\">\"abb\""
                (shouldReturn
                   (stepDefaultedTextly "\"abc\">\"abb\"")
                   (Right "#true"))
              it
                "\"x\"<=\"x\""
                (shouldReturn (stepDefaultedTextly "\"x\"<=\"x\"") (Right "#true"))
              it
                "\"x\">\"x\""
                (shouldReturn (stepDefaultedTextly "\"x\">\"x\"") (Right "#false"))
              it
                "\"x\">\"y\""
                (shouldReturn (stepDefaultedTextly "\"x\">\"y\"") (Right "#false"))))

quoteBool :: Bool -> Text
quoteBool True = "#true"
quoteBool False = "#false"

functions :: SpecWith ()
functions =
  describe
    "Functions"
    (do functions1;functions2)

functions2 :: SpecWith ()
functions2 = do
  describe
    "all"
    (do it
          "all(_, [1])"
          (shouldReturn
             (stepDefaultedTextly "all(_, [1])")
             (Right "all(_, [1])"))
        it
          "all(_, [])"
          (shouldReturn
             (stepDefaultedTextly "all(_, [])")
             (Right "#none"))
        it
          "all(r:r=2, _)"
          (shouldReturn
             (stepDefaultedTextly "all(r:r=2,_)")
             (Right "all(:($0 = fromInteger(2)), _)"))
        it
          "all(r:r=2,[2,2,_])"
          (shouldReturn
             (stepDefaultedTextly "all(r:r=2,[2,2,_])")
             (Right "all(:($0 = fromInteger(2)), [2, 2, _])"))
        it
          "all(r:r=2,[2,2,2])"
          (shouldReturn
             (stepDefaultedTextly "all(r:r=2,[2,2,2])")
             (Right "#ok(#true)"))
        it
          "all(r:r=2,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "all(r:r=2,[1,2,3])")
             (Right "#ok(#false)"))
        it
          "all(r:r=6,[])"
          (shouldReturn
             (stepDefaultedTextly "all(r:r=6,[])")
             (Right "#none")))
  describe
    "any"
    (do it
          "any(r:r=2,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "any(r:r=2,[1,2,3])")
             (Right "#ok(#true)"))
        it
          "any(r:r=6,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "any(r:r=6,[1,2,3])")
             (Right "#ok(#false)"))
        it
          "any(r:r=6,[])"
          (shouldReturn
             (stepDefaultedTextly "any(r:r=6,[])")
             (Right "#none")))

functions1 :: Spec
functions1 = do
  describe
    "map"
    (do it
          "map(r:r*2,[])"
          (shouldReturn
             (stepDefaultedTextly "map(r:r*2,[])")
             (Right "[]"))
        it
          "map(r:r*2,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "map(r:r*2,[1,2,3])")
             (Right "[2, 4, 6]"))
        it
          "map(r:r.x*2,[{x:1},{x:2},{x:3}])"
          (shouldReturn
             (stepDefaultedTextly "map(r:r.x*2,[{x:1},{x:2},{x:3}])")
             (Right "[2, 4, 6]")))
  describe
    "find"
    (do it
          "find(r:r.name=\"jane\",[])"
          (shouldReturn
             (stepDefaultedTextly "find(r:r.name=\"jane\",[])")
             (Right "#none"))
        it
          "find(r:r.name=\"jane\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])"
          (shouldReturn
             (stepDefaultedTextly
                "find(r:r.name=\"jane\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])")
             (Right "#ok({\"name\": \"jane\"})"))
        it
          "find(r:r.name=\"janet\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])"
          (shouldReturn
             (stepDefaultedTextly
                "find(r:r.name=\"janet\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])")
             (Right "#none"))
        it
          "find(x:x>4,[1,2,3,4,5,6,7])"
          (shouldReturn
             (stepDefaultedTextly "find(x:x>4,[1,2,3,4,5,6,7])")
             (Right "#ok(5)"))
        it
          "find(r:r.x>=2, [{x:1},{x:2},{x:3}])"
          (shouldReturn
             (stepDefaultedTextly "find(r:r.x>=2, [{x:1},{x:2},{x:3}])")
             (Right "#ok({\"x\": 2})")))
  describe
    "filter"
    (do it
          "filter(r:r.x>=2, [{x:1},{x:2},{x:3}])"
          (shouldReturn
             (stepDefaultedTextly "filter(r:r.x>=2, [{x:1},{x:2},{x:3}])")
             (Right "[{\"x\": 2}, {\"x\": 3}]")))
  describe
    "length"
    (do it
          "length([])"
          (shouldReturn (stepDefaultedTextly "length([])") (Right "0"))
        it
          "length([1,2,3])"
          (shouldReturn (stepDefaultedTextly "length([1,2,3])") (Right "3"))
        it
          "null([])"
          (shouldReturn (stepDefaultedTextly "null([])") (Right "#true"))
        it
          "null([1,2,3])"
          (shouldReturn (stepDefaultedTextly "null([1,2,3])") (Right "#false")))
  describe
    "sum"
    (do it
          "sum([1,2,3])"
          (shouldReturn (stepDefaultedTextly "sum([1,2,3])") (Right "#ok(6)"))
        it
          "sum(filter(x:x>5,[1,2,3]))"
          (shouldReturn
             (stepDefaultedTextly "sum(filter(x:x>5,[1,2,3]))")
             (Right "#none"))
        it
          "sum([])"
          (shouldReturn (stepDefaultedTextly "sum([])") (Right "#none"))
        it
          "sum(map(x:x.p,[{p:1},{p:2}]))"
          (shouldReturn
             (stepDefaultedTextly "sum(map(x:x.p,[{p:1},{p:2}]))")
             (Right "#ok(3)"))
        it
          "sum(map(x:x.p,[{p:1.0},{p:-2.2}]))"
          (shouldReturn
             (stepDefaultedTextly "sum(map(x:x.p,[{p:1.0},{p:-2.2}]))")
             (Right "#ok(-1.2)")))
  describe
    "average"
    (do it
          "average([24 , 55 , 17 , 87 , 100])"
          (shouldReturn
             (stepDefaultedTextly "average([24 , 55 , 17 , 87 , 100])")
             (Right "#ok(56)"))
        it
          "average([24 , 55 , 17 , 87 , 100.0])"
          (shouldReturn
             (stepDefaultedTextly "average([24 , 55 , 17 , 87 , 100.0])")
             (Right "#ok(56.6)"))
        it
          "average(filter(x:x>5,[1,2,3]))"
          (shouldReturn
             (stepDefaultedTextly "average(filter(x:x>5,[1,2,3]))")
             (Right "#none"))
        it
          "average([])"
          (shouldReturn (stepDefaultedTextly "average([])") (Right "#none"))
        it
          "average(map(x:x.p,[{p:1},{p:2}]))"
          (shouldReturn
             (stepDefaultedTextly "average(map(x:x.p,[{p:1},{p:2.0}]))")
             (Right "#ok(1.5)"))
        it
          "average(map(x:x.p,[{p:1.0},{p:-2.2}]))"
          (shouldReturn
             (stepDefaultedTextly "average(map(x:x.p,[{p:1.0},{p:-2.2}]))")
             (Right "#ok(-0.6)")))
  describe
    "distinct"
    (do it
          "distinct([])"
          (shouldReturn (stepDefaultedTextly "distinct([])") (Right "[]"))
        it
          "distinct([1,2,1,3,3,3,1])"
          (shouldReturn
             (stepDefaultedTextly "distinct([1,2,1,3,3,3,1])")
             (Right "[1, 2, 3]")))
  describe
    "sort"
    (do it
          "sort([])"
          (shouldReturn (stepDefaultedTextly "sort([])") (Right "[]"))
        it
          "sort([5,3,8,2,4,2,9,1])"
          (shouldReturn
             (stepDefaultedTextly "sort([5,3,8,2,4,2,9,1])")
             (Right "[1, 2, 2, 3, 4, 5, 8, 9]")))
  describe
    "maximum"
    (do it
          "maximum([])"
          (shouldReturn (stepDefaultedTextly "maximum([])") (Right "#none"))
        it
          "maximum([2,3,1,5])"
          (shouldReturn
             (stepDefaultedTextly "maximum([2,3,1,5])")
             (Right "#ok(5)")))
  describe
    "minimum"
    (do it
          "minimum([])"
          (shouldReturn (stepDefaultedTextly "minimum([])") (Right "#none"))
        it
          "minimum([2,3,1,5])"
          (shouldReturn
             (stepDefaultedTextly "minimum([2,3,1,5])")
             (Right "#ok(1)")))

if' :: SpecWith ()
if' =
  describe
    "If"
    (do it
          "if #false then 1 else 0"
          (shouldReturn (stepDefaultedTextly "if #false then 1 else 0")
                    (Right "0"))
        it
          "if #true then 1 else 0"
          (shouldReturn (stepDefaultedTextly "if #true then 1 else 0")
                    (Right "1"))
        it
          "if 2*2>1 then 1 else 0"
          (shouldReturn (stepDefaultedTextly "if 2>1 then 1 else 0")
                    (Right "1"))
        it
          "if 2>4 then 1 else 0"
          (shouldReturn (stepDefaultedTextly "if 2>4 then 1 else 0")
                    (Right "0")))

case' :: SpecWith ()
case' =
  describe
    "Case"
    (do it
          "case 2>4 { #true: 1, #false: 0 }"
          (shouldReturn
             (stepDefaultedTextly "case 2>4 { #true: 1, #false: 0 }")
             (Right "0"))
        it
          "case 2>1 { #true: 1, #false: 0 }"
          (shouldReturn
             (stepDefaultedTextly "case 2>1 { #true: 1, #false: 0 }")
             (Right "1"))
        it
          "case #ok(1) { #ok(n): n, #none: 0 }"
          (shouldReturn
             (stepDefaultedTextly "case #ok(1) { #ok(n): n, #none: 0 }")
             (Right "1"))
        it
          "case #none { #ok(n): n, #none: 0 }"
          (shouldReturn
             (stepDefaultedTextly "case #none { #ok(n): n, #none: 0 }")
             (Right "0"))
        it
          "case #ok(2*3) { #ok(n): n, #none: 0 }"
          (shouldReturn
             (stepDefaultedTextly "case #ok(2*3) { #ok(n): n, #none: 0 }")
             (Right "6"))
        -- This sneaky test checks that all the types unify properly and defaulting working:
        it
          "case #ok(2*3) { #ok(n): n, #none: 0.0 } -- check unification"
          (shouldReturn
             (stepDefaultedTextly "case #ok(2*3) { #ok(n): n, #none: 0.0 }")
             (Right "6.0"))
        it
          "nested cases"
          (shouldReturn
             (stepDefaultedTextly
                "case 2>4 { #true: \"early\", #false: case 2=2 { #true: \"ok\", #false: \"nope\" } }")
             (Right "\"ok\"")))

early :: Spec
early =
  describe
    "Early"
    (do it
          "#none? + 1"
          (shouldReturn
             (stepDefaultedTextly "#none? + 1 + #wibble?")
             (Right "#none"))
        it
          "#ok(3)? + 1"
          (shouldReturn
             (stepDefaultedTextly "#ok(3)? + 1")
             (Right "#ok(4)"))
        it
          "#ok(3)? + 1 + #ok(4)?"
          (shouldReturn
             (stepDefaultedTextly "#ok(3)? + 1 + #ok(4)?")
             (Right "#ok(8)"))
        it
          "find(x:x>5,[5,2,63,1,3])? + 3"
          (shouldReturn
             (stepDefaultedTextly "find(x:x>5,[5,2,63,1,3])? + 3")
             (Right "#ok(66)"))
        it
          "find(x:x>5,[5,2,63,1,3])? + 3 + #oops?"
          (shouldReturn
             (stepDefaultedTextly "find(x:x>5,[5,2,63,1,3])? + 3 + #oops?")
             (Right "#oops"))
        it
          "case (x: find(x:x>5,[5,2,63,1,3])? + 3 + #oops?)({}) { #oops: 1, #ok(n): n, #none: 0 }"
          (shouldReturn
             (stepDefaultedTextly "case (x: find(x:x>5,[5,2,63,1,3])? + 3 + #oops?)({}) { #oops: 1, #ok(n): n, #none: 0 }")
             (Right "1")))
