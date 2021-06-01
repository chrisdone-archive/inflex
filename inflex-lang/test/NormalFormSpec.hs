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
    (do describe "One-way" oneway
        describe "Inferred" inferred)
  describe "Erroring" erroring

inferred :: Spec
inferred = do
  it
    "array a vs array Nothing"
    (shouldBe
       (fmap expressionGenerate (parseText "" "[[1],[]]"))
       (Right (Right (ArrayT (Just (ArrayT (Just IntegerT)))))))
  it
    "[\"foo\",\"bar\"]"
    (shouldBe
       (fmap expressionGenerate (parseText "" "[\"foo\",\"bar\"]"))
       (Right (Right (ArrayT (Just TextT)))))
  it
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
    "{x:1,y:\"a\",z:[],q:1.2}"
    (shouldBe
       (fmap expressionGenerate (parseText "" "{x:1,y:\"a\",z:[],q:1.2}"))
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
          (parseText "" "[{x:1,y:\"a\",z:[],q:1.2},{q:1.2,z:[],x:1,y:\"a\"}]"))
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
          (parseText "" "[{x:1,y:\"a\",z:[],q:1.2},{q:1.2,z:[],x:1.00,y:\"a\"}]"))
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
                         ])))))))

oneway :: Spec
oneway = do
  it
    "promotion of integer to decimal"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[1] :: [Decimal 2]"))
       (Right (Right (ArrayT (Just (DecimalT 2))))))
  it
    "check wider signature is fine"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[1.23, 1.23] :: [Decimal 3]"))
       (Right (Right (ArrayT (Just (DecimalT 3))))))
  it
    "check varying precisions is fine"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[1.23, 1.2] :: [Decimal 2]"))
       (Right (Right (ArrayT (Just (DecimalT 2))))))
  it
    "[\"foo\",\"bar\"] :: [Text]"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[\"foo\",\"bar\"] :: [Text]"))
       (Right (Right (ArrayT (Just TextT)))))
  it
    "[#ok(1)] :: [<ok:Integer|_>]"
    (do shouldBe
          (fmap resolveParsedT (parseText "" "[#ok(1)] :: [<ok:Integer|_>]"))
          (Right
             (Right
                (ArrayT
                   (pure
                      (VariantT
                         (OM.singleton (TagName {unTagName = "ok"}) IntegerT)))))))
  it
    "variants varying"
    (shouldBe
       (fmap
          resolveParsedT
          (parseText "" "[#ok(1.1),#none] :: [<ok:Decimal 2,none:{}|_>]"))
       (Right
          (Right
             (ArrayT
                (Just
                   (VariantT
                      (OM.fromList
                         [ (TagName {unTagName = "ok"}, DecimalT 2)
                         , (TagName {unTagName = "none"}, RecordT mempty)
                         ])))))))
  it
    "variants and records combined"
    (shouldBe
       (fmap
          resolveParsedT
          (parseText "" "[{x:#ok(1)},{x:#ok(4)}] :: [{x:<ok:Integer|_>}]"))
       (Right
          (Right
             (ArrayT
                (Just
                   (RecordT
                      (OM.fromList
                         [ ( FieldName {unFieldName = "x"}
                           , VariantT
                               (OM.fromList
                                  [(TagName {unTagName = "ok"}, IntegerT)]))
                         ])))))))
  it
    "signature for list of records"
    (shouldBe
       (fmap
          resolveParsedT
          (parseText
             ""
             "[{x:1,y:\"a\",q:1.2}] :: [{x:Integer,y:Text,q:Decimal 2}]"))
       (Right
          (Right
             (ArrayT
                (Just
                   (RecordT
                      (OM.fromList
                         [ (FieldName {unFieldName = "q"}, DecimalT 2)
                         , (FieldName {unFieldName = "x"}, IntegerT)
                         , (FieldName {unFieldName = "y"}, TextT)
                         ])))))))

erroring :: Spec
erroring = do
  it
    "couldn't intern type"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[1.23, 1.203] :: _"))
       (Right (Left CouldntInternType)))
  it
    "no type sig"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[1.23, 1.203]"))
       (Right (Left NoTypeSig)))
  it
    "[1.23, 1.203] :: [Decimal 2]"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[1.23, 1.203] :: [Decimal 2]"))
       (Right (Left (TypeMismatch (DecimalT 2) (DecimalT 3)))))
  it
    "[1.1] :: [Integer]"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[1.1] :: [Integer]"))
       (Right (Left (TypeMismatch IntegerT (DecimalT 1)))))
  it
    "[#ok(1.1)] :: [<ok:Integer|_>]"
    (shouldBe
       (fmap resolveParsedT (parseText "" "[#ok(1.1)] :: [<ok:Integer|_>]"))
       (Right (Left (TypeMismatch IntegerT (DecimalT 1)))))
  it
    "differing labels of same name"
    (shouldBe
       (fmap
          resolveParsedT
          (parseText
             ""
             "[{x:#ok(1)},{x:#ok(\"wibble\")}] :: [{x:<ok:Integer|_>}]"))
       (Right (Left (TypeMismatch IntegerT TextT))))
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
          (parseText "" "[{y:1,x:\"a\",q:[],z:1.2},{q:1.2,z:[],x:1,y:\"a\"}]"))
       (Right (Left (TypeMismatch (DecimalT 1) (ArrayT Nothing)))))
  it
    "[{y:1,x:\"a\",q:[],z:1.2},{q:1.2,z:[],x:1,y:\"a\"}]"
    (shouldBe
       (fmap
          resolveParsedT
          (parseText "" "[{y:1,x:\"a\"},{y:1,k:\"a\"}]::[{y:Integer,x:Text}]"))
       (Right
          (Left
             (RecordFieldsMismatch
                [FieldName {unFieldName = "y"}, FieldName {unFieldName = "x"}]
                [FieldName {unFieldName = "y"}, FieldName {unFieldName = "k"}]))))
  it
    "fields mismatch for unifyT"
    (shouldBe
       (fmap
          expressionGenerate
          (parseText "" "[{x:1,y:\"a\",z:[],q:1.2},{q:1.2,z:[],x:1,k:\"a\"}]"))
       (Right
          (Left
             (RecordFieldsMismatch
                [ FieldName {unFieldName = "x"}
                , FieldName {unFieldName = "y"}
                , FieldName {unFieldName = "z"}
                , FieldName {unFieldName = "q"}
                ]
                [ FieldName {unFieldName = "q"}
                , FieldName {unFieldName = "z"}
                , FieldName {unFieldName = "x"}
                , FieldName {unFieldName = "k"}
                ]))))
