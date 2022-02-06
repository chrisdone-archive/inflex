{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
-- |

module StepSpec where

import           Data.Fixed
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Eval
import           Inflex.Printer
import           Inflex.Types
import qualified Match as Match
import qualified RIO
import           Test.Hspec
import           Test.QuickCheck

stepDefaultedTextly :: Text -> IO (Either (DefaultEvalError ()) Text)
stepDefaultedTextly text' =
  RIO.runRIO
    Eval {glogfunc = mempty, globals = mempty}
    (fmap (fmap (printerText emptyPrinterConfig)) (evalTextDefaulted mempty "" text'))

stepTextly :: Text -> IO (Either (DefaultEvalError ()) Text)
stepTextly text' =
  RIO.runRIO
    Eval {glogfunc = mempty, globals = mempty}
    (fmap (fmap (printerText emptyPrinterConfig)) (evalTextDefaulted mempty "" text'))

stepDefaulted' :: Text -> IO (Either (DefaultEvalError ()) (Expression Resolved))
stepDefaulted' text' =
  RIO.runRIO
    Eval {glogfunc = mempty, globals = mempty}
    (evalTextDefaulted mempty "" text')

spec :: SpecWith ()
spec = do
  describe
    "Single expressions"
    (do -- Rich text
        let asis k = it (T.unpack k) (shouldReturn (stepTextly k) (Right k))
        asis "@prim:rich_doc([@prim:rich_paragraph([@prim:rich_text(\"Hello!\")])])"
        asis "@prim:rich_doc([@prim:rich_paragraph([@prim:rich_bold(@prim:rich_text(\"Hello!\"))])])"

        it "6" (shouldReturn (stepTextly "6 :: Integer") (Right "6"))
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
          "@prim:from_integer (6 :: Integer) + 3.10 + @prim:from_decimal (3.1 :: Decimal 1)"
          (shouldReturn
             (stepDefaultedTextly
                "@prim:from_integer (6 :: Integer) + 3.10 + @prim:from_decimal (3.1 :: Decimal 1)")
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
  it
    "@prim:from_ok(0,#ok(2)) + 1"
    (shouldReturn (stepDefaultedTextly "@prim:from_ok(0,#ok(2)) + 1") (Right "3"))
  it
    "@prim:from_ok(0,#oops) + 1"
    (shouldReturn (stepDefaultedTextly "@prim:from_ok(0,#oops) + 1") (Right "1"))
  regression

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
    "@prim:array_all"
    (do it
          "@prim:array_all(_, [1])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_all(_, [1])")
             (Right "if @prim:array_find(($:@prim:not(_($0))), [1]) {#find_empty: #all_empty, #find_failed: #ok(#true), #ok($): #ok(#false)}"))
        it
          "@prim:array_all(_, [])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_all(_, [])")
             (Right "#all_empty"))
        it
          "@prim:array_all(r:r=2, _)"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_all(r:r=2,_)")
             (Right "if @prim:array_find(($:@prim:not(($:($0 = 2))($0))), _) {#find_empty: #all_empty, #find_failed: #ok(#true), #ok($): #ok(#false)}"))
        it
          "@prim:array_all(r:r=2,[2,2,_])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_all(r:r=2,[2,2,_])")
             (Right "if @prim:array_find(($:@prim:not(($:($0 = 2))($0))), [2, 2, _]) {#find_empty: #all_empty, #find_failed: #ok(#true), #ok($): #ok(#false)}"))
        it
          "@prim:array_all(r:r=2,[2,2,2])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_all(r:r=2,[2,2,2])")
             (Right "#ok(#true)"))
        it
          "@prim:array_all(r:r=2,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_all(r:r=2,[1,2,3])")
             (Right "#ok(#false)"))
        it
          "@prim:array_all(r:r=6,[])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_all(r:r=6,[])")
             (Right "#all_empty")))
  describe
    "@prim:array_any"
    (do it
          "@prim:array_any(r:r=2,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_any(r:r=2,[1,2,3])")
             (Right "#ok(#true)"))
        it
          "@prim:array_any(r:r=6,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_any(r:r=6,[1,2,3])")
             (Right "#ok(#false)"))
        it
          "@prim:array_any(r:r=6,[])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_any(r:r=6,[])")
             (Right "#any_empty")))

functions1 :: Spec
functions1 = do
  describe
    "@prim:array_map"
    (do it
          "@prim:array_map(r:r*2,[])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_map(r:r*2,[])")
             (Right "[]"))
        it
          "@prim:array_map(r:r*2,[1,2,3])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_map(r:r*2,[1,2,3])")
             (Right "[2, 4, 6]"))
        it
          "@prim:array_map(r:r.x*2,[{x:1},{x:2},{x:3}])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_map(r:r.x*2,[{x:1},{x:2},{x:3}])")
             (Right "[2, 4, 6]")))
  describe
    "@prim:array_find"
    (do it
          "@prim:array_find(r:r.name=\"jane\",[])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_find(r:r.name=\"jane\",[])")
             (Right "#find_empty"))
        it
          "@prim:array_find(r:r.name=\"jane\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])"
          (shouldReturn
             (stepDefaultedTextly
                "@prim:array_find(r:r.name=\"jane\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])")
             (Right "#ok({\"name\": \"jane\"})"))
        it
          "@prim:array_find(r:r.name=\"janet\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])"
          (shouldReturn
             (stepDefaultedTextly
                "@prim:array_find(r:r.name=\"janet\",[{\"name\":\"mary\"},{\"name\":\"jane\"}])")
             (Right "#find_failed"))
        it
          "@prim:array_find(x:x>4,[1,2,3,4,5,6,7])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_find(x:x>4,[1,2,3,4,5,6,7])")
             (Right "#ok(5)"))
        it
          "@prim:array_find(r:r.x>=2, [{x:1},{x:2},{x:3}])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_find(r:r.x>=2, [{x:1},{x:2},{x:3}])")
             (Right "#ok({\"x\": 2})")))
  describe
    "@prim:array_filter"
    (do it
          "@prim:array_filter(r:r.x>=2, [{x:1},{x:2},{x:3}])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_filter(r:r.x>=2, [{x:1},{x:2},{x:3}])")
             (Right "[{\"x\": 2}, {\"x\": 3}]")))
  describe
    "@prim:array_concat"
    (do it
          "@prim:array_concat(...)"
          (do shouldReturn
                (stepDefaultedTextly "@prim:array_concat([[1,2,3],[4,5,6],[],[7,8]])")
                (Right "[1, 2, 3, 4, 5, 6, 7, 8]")
              shouldReturn
                (stepDefaultedTextly "@prim:array_concat([])")
                (Right "[]")))
  describe
    "@prim:array_length"
    (do it
          "@prim:array_length([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_length([])") (Right "0"))
        it
          "@prim:array_length([1,2,3])"
          (shouldReturn (stepDefaultedTextly "@prim:array_length([1,2,3])") (Right "3"))
        it
          "@prim:array_null([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_null([])") (Right "#true"))
        it
          "@prim:array_null([1,2,3])"
          (shouldReturn (stepDefaultedTextly "@prim:array_null([1,2,3])") (Right "#false")))
  describe
    "@prim:array_sum"
    (do it
          "@prim:array_sum([1,2,3])"
          (shouldReturn (stepDefaultedTextly "@prim:array_sum([1,2,3])") (Right "#ok(6)"))
        it
          "@prim:array_sum(@prim:array_filter(x:x>5,[1,2,3]))"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_sum(@prim:array_filter(x:x>5,[1,2,3]))")
             (Right "#sum_empty"))
        it
          "@prim:array_sum([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_sum([])") (Right "#sum_empty"))
        it
          "@prim:array_sum(@prim:array_map(x:x.p,[{p:1},{p:2}]))"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_sum(@prim:array_map(x:x.p,[{p:1},{p:2}]))")
             (Right "#ok(3)"))
        it
          "@prim:array_sum(@prim:array_map(x:x.p,[{p:1.0},{p:-2.2}]))"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_sum(@prim:array_map(x:x.p,[{p:1.0},{p:-2.2}]))")
             (Right "#ok(-1.2)")))
  describe
    "@prim:array_average"
    (do it
          "@prim:array_average([24 , 55 , 17 , 87 , 100])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_average([24 , 55 , 17 , 87 , 100])")
             (Right "#ok(56)"))
        it
          "@prim:array_average([24 , 55 , 17 , 87 , 100.0])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_average([24 , 55 , 17 , 87 , 100.0])")
             (Right "#ok(56.6)"))
        it
          "@prim:array_average(@prim:array_filter(x:x>5,[1,2,3]))"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_average(@prim:array_filter(x:x>5,[1,2,3]))")
             (Right "#average_empty"))
        it
          "@prim:array_average([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_average([])") (Right "#average_empty"))
        it
          "@prim:array_average(@prim:array_map(x:x.p,[{p:1},{p:2}]))"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_average(@prim:array_map(x:x.p,[{p:1},{p:2.0}]))")
             (Right "#ok(1.5)"))
        it
          "@prim:array_average(@prim:array_map(x:x.p,[{p:1.0},{p:-2.2}]))"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_average(@prim:array_map(x:x.p,[{p:1.0},{p:-2.2}]))")
             (Right "#ok(-0.6)")))
  describe
    "@prim:array_distinct"
    (do it
          "@prim:array_distinct([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_distinct([])") (Right "[]"))
        it
          "@prim:array_distinct([1,2,1,3,3,3,1])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_distinct([1,2,1,3,3,3,1])")
             (Right "[1, 2, 3]")))
  describe
    "@prim:array_sort"
    (do it
          "@prim:array_sort([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_sort([])") (Right "[]"))
        it
          "@prim:array_sort([5,3,8,2,4,2,9,1])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_sort([5,3,8,2,4,2,9,1])")
             (Right "[1, 2, 2, 3, 4, 5, 8, 9]")))
  describe
    "@prim:array_maximum"
    (do it
          "@prim:array_maximum([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_maximum([])") (Right "#maximum_empty"))
        it
          "@prim:array_maximum([2,3,1,5])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_maximum([2,3,1,5])")
             (Right "#ok(5)")))
  describe
    "@prim:array_minimum"
    (do it
          "@prim:array_minimum([])"
          (shouldReturn (stepDefaultedTextly "@prim:array_minimum([])") (Right "#minimum_empty"))
        it
          "@prim:array_minimum([2,3,1,5])"
          (shouldReturn
             (stepDefaultedTextly "@prim:array_minimum([2,3,1,5])")
             (Right "#ok(1)")))

if' :: SpecWith ()
if' =
  describe
    "If"
    (do it
          "if #false { #true: 1, _: 0 }"
          (shouldReturn (stepDefaultedTextly "if #false { #true: 1, _: 0 }")
                    (Right "0"))
        it
          "if #true { #true: 1, _: 0 }"
          (shouldReturn (stepDefaultedTextly "if #true { #true: 1, _: 0 }")
                    (Right "1"))
        it
          "if 2*2>1 { #true: 1, _: 0 }"
          (shouldReturn (stepDefaultedTextly "if 2*2>1 { #true: 1, _: 0 }")
                    (Right "1"))
        it
          "if 2>4 { #true: 1, _: 0 }"
          (shouldReturn (stepDefaultedTextly "if 2>4 { #true: 1, _: 0 }")
                    (Right "0")))

case' :: SpecWith ()
case' =
  describe
    "If"
    (do it
          "if 2>4 { #true: 1, #false: 0 }"
          (shouldReturn
             (stepDefaultedTextly "if 2>4 { #true: 1, #false: 0 }")
             (Right "0"))
        it
          "if 2>1 { #true: 1, #false: 0 }"
          (shouldReturn
             (stepDefaultedTextly "if 2>1 { #true: 1, #false: 0 }")
             (Right "1"))
        it
          "if #ok(1) { #ok(n): n, #none: 0 }"
          (shouldReturn
             (stepDefaultedTextly "if #ok(1) { #ok(n): n, #none: 0 }")
             (Right "1"))
        it
          "if #none { #ok(n): n, #none: 0 }"
          (shouldReturn
             (stepDefaultedTextly "if #none { #ok(n): n, #none: 0 }")
             (Right "0"))
        it
          "if #ok(2*3) { #ok(n): n, #none: 0 }"
          (shouldReturn
             (stepDefaultedTextly "if #ok(2*3) { #ok(n): n, #none: 0 }")
             (Right "6"))
        -- This sneaky test checks that all the types unify properly and defaulting working:
        it
          "if #ok(2*3) { #ok(n): n, #none: 0.0 } -- check unification"
          (shouldReturn
             (stepDefaultedTextly "if #ok(2*3) { #ok(n): n, #none: 0.0 }")
             (Right "6.0"))
        it
          "nested ifs"
          (shouldReturn
             (stepDefaultedTextly
                "if 2>4 { #true: \"early\", #false: if 2=2 { #true: \"ok\", #false: \"nope\" } }")
             (Right "\"ok\"")))

regression :: SpecWith ()
regression =
  describe
    "Regression tests"
    (do it
          "filter of filter yields correct type"
          -- In this example the `x' field was being lost in the return type.
          (do result <-
                stepDefaulted'
                  "@prim:array_filter(x:x.Amount<10000,@prim:array_filter(x:x.Amount>1000,[{Amount:9999,x:\"\"}]))"
              shouldSatisfy
                result
                $(Match.match
                    [|Right
                        (ArrayExpression
                           (Array
                              { expressions =
                                  [ RecordExpression
                                      (Record
                                         { fields =
                                             [ FieldE
                                                 { name =
                                                     FieldName
                                                       {unFieldName = "Amount"}
                                                 }
                                             , FieldE
                                                 { name =
                                                     FieldName
                                                       {unFieldName = "x"}
                                                 }
                                             ]
                                         , typ =
                                             RecordType
                                               (RowType
                                                  (TypeRow
                                                     { typeVariable = Nothing
                                                     , fields =
                                                         [ Field
                                                             { name =
                                                                 FieldName
                                                                   { unFieldName =
                                                                       "Amount"
                                                                   }
                                                             }
                                                         , Field
                                                             { name =
                                                                 FieldName
                                                                   { unFieldName =
                                                                       "x"
                                                                   }
                                                             }
                                                         ]
                                                     }))
                                         })
                                  ]
                              , typ =
                                  ArrayType
                                    (RecordType
                                       (RowType
                                          (TypeRow
                                             { typeVariable = Nothing
                                             , fields =
                                                 [ Field
                                                     { name =
                                                         FieldName
                                                           {unFieldName = "x"}
                                                     }
                                                 , Field
                                                     { name =
                                                         FieldName
                                                           { unFieldName =
                                                               "Amount"
                                                           }
                                                     }
                                                 ]
                                             })))
                              }))|]))
        it
          "concat of map yields correct type"
          (do result <-
                stepDefaulted'
                  "@prim:array_concat( @prim:array_map(xs:@prim:array_map(x:{x:x.a},xs),[[{a:1},{a:2}],[{a:3},{a:4}]]) )"
              shouldSatisfy
                result
                $(Match.match
                    [|Right
                        (ArrayExpression
                           (Array
                              { expressions =
                                  [ RecordExpression
                                      (Record
                                         { fields =
                                             [ FieldE
                                                 { name =
                                                     FieldName
                                                       {unFieldName = "x"}
                                                 }
                                             ]
                                         , typ =
                                             RecordType
                                               (RowType
                                                  (TypeRow
                                                     { typeVariable = Nothing
                                                     , fields =
                                                         [ Field
                                                             { name =
                                                                 FieldName
                                                                   { unFieldName =
                                                                       "x"
                                                                   }
                                                             , typ =
                                                                 PolyType
                                                                   (TypeVariable
                                                                      { location =
                                                                          ()
                                                                      , prefix =
                                                                          ()
                                                                      , index =
                                                                          0
                                                                      , kind =
                                                                          TypeKind
                                                                      })
                                                             }
                                                         ]
                                                     }))
                                         })
                                  , RecordExpression _
                                  , RecordExpression _
                                  , RecordExpression _
                                  ]
                              , typ =
                                  ArrayType
                                    (RecordType
                                       (RowType
                                          (TypeRow
                                             { typeVariable = Nothing
                                             , fields =
                                                 [ Field
                                                     { name =
                                                         FieldName
                                                           {unFieldName = "x"}
                                                     , typ =
                                                         PolyType
                                                           (TypeVariable
                                                              { location = ()
                                                              , prefix = ()
                                                              , index = 0
                                                              , kind = TypeKind
                                                              })
                                                     }
                                                 ]
                                             })))
                              }))|])))
