{-# LANGUAGE OverloadedStrings #-}
-- |

module NormalFormSpec where

import qualified Data.HashMap.Strict.InsOrd as OM
import           Inflex.NormalFormCheck
import           Inflex.Parser
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe
    "Succeeding"
    (do it
          "[1,2,3]"
          (shouldBe
             (fmap expressionGenerate (parseText "" "[1,2,3]"))
             (Right (Right (ArrayT (Just IntegerT)))))
        it
          "[1.0,2,3.000]"
          (shouldBe
             (fmap expressionGenerate (parseText "" "[1.0,2,3.000]"))
             (Right (Right (ArrayT (Just (DecimalT 3))))))
        it
          "[[1],[2],[3]]"
          (shouldBe
             (fmap expressionGenerate (parseText "" "[[1],[2],[3]]"))
             (Right (Right (ArrayT (Just (ArrayT (Just IntegerT)))))))
        it
          "[#ok(1)] :: [<ok:Integer|_>]"
          (do pending
              shouldBe
                (fmap
                   resolveParsedT
                   (parseText "" "[#ok(1)] :: [<ok:Integer|_>]"))
                (Right
                   (Right
                      (ArrayT
                         (pure
                            (VariantT (TagName {unTagName = "ok"}) (Just IntegerT)))))))
        it
          "{x:1,y:\"a\",z:[],q:1.2}"
          (shouldBe
             (fmap
                expressionGenerate
                (parseText "" "{x:1,y:\"a\",z:[],q:1.2}"))
             (Right
                (Right
                   (RecordT
                      (OM.fromList
                         [ (FieldName {unFieldName = "z"}, ArrayT Nothing)
                         , (FieldName {unFieldName = "q"}, DecimalT 1)
                         , (FieldName {unFieldName = "x"}, IntegerT)
                         , (FieldName {unFieldName = "y"}, TextT)
                         ])))))
        it
          "[{x:1,y:\"a\",z:[],q:1.2},{q:1.2,z:[],x:1,y:\"a\"}]"
          (shouldBe
             (fmap
                expressionGenerate
                (parseText
                   ""
                   "[{x:1,y:\"a\",z:[],q:1.2},{q:1.2,z:[],x:1,y:\"a\"}]"))
             (Right
                (Right
                   (ArrayT
                      (Just
                         (RecordT
                            (OM.fromList
                               [ (FieldName {unFieldName = "z"}, ArrayT Nothing)
                               , (FieldName {unFieldName = "q"}, DecimalT 1)
                               , (FieldName {unFieldName = "x"}, IntegerT)
                               , (FieldName {unFieldName = "y"}, TextT)
                               ])))))))
        it
          "[{x:1,y:\"a\",z:[],q:1.2},{q:1.2,z:[],x:1.00,y:\"a\"}]"
          (shouldBe
             (fmap
                expressionGenerate
                (parseText
                   ""
                   "[{x:1,y:\"a\",z:[],q:1.2},{q:1.2,z:[],x:1.00,y:\"a\"}]"))
             (Right
                (Right
                   (ArrayT
                      (Just
                         (RecordT
                            (OM.fromList
                               [ (FieldName {unFieldName = "z"}, ArrayT Nothing)
                               , (FieldName {unFieldName = "q"}, DecimalT 1)
                               , (FieldName {unFieldName = "x"}, DecimalT 2)
                               , (FieldName {unFieldName = "y"}, TextT)
                               ]))))))))
  describe
    "Erroring"
    (do it
          "[1.1] :: [Integer]"
          (do pending
              shouldBe
                (fmap resolveParsedT (parseText "" "[1.1] :: [Integer]"))
                (Right (Right (ArrayT (Just IntegerT)))))
        it
          "[#ok(1.1)] :: [<ok:Integer|_>]"
          (do pending
              shouldBe
                (fmap
                   resolveParsedT
                   (parseText "" "[#ok(1.1)] :: [<ok:Integer|_>]"))
                (Right
                   (Right
                      (ArrayT
                         (pure
                            (VariantT (TagName {unTagName = "ok"}) (Just IntegerT)))))))
        it
          "[#ok(1.1),#none] :: [<ok:Integer,none:{}|_>]"
          (do pending
              shouldBe
                (fmap
                   resolveParsedT
                   (parseText "" "[#ok(1.1)] :: [<ok:Integer|_>]"))
                (Right
                   (Right
                      (ArrayT
                         (pure
                            (VariantT (TagName {unTagName = "ok"}) (Just IntegerT)))))))
        it
          "[1,\"woo\",3]"
          (shouldBe
             (fmap expressionGenerate (parseText "" "[1,\"woo\",3]"))
             (Right (Left (TypeMismatch IntegerT TextT))))
        it
          "[{y:1,x:\"a\",q:[],z:1.2},{q:1.2,z:[],x:1,y:\"a\"}]"
          (shouldBe
             (fmap
                expressionGenerate
                (parseText
                   ""
                   "[{y:1,x:\"a\",q:[],z:1.2},{q:1.2,z:[],x:1,y:\"a\"}]"))
             (Right (Left (TypeMismatch (DecimalT 1) (ArrayT Nothing))))))
