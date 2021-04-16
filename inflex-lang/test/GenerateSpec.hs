{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Tests for generation of type constraints.

module GenerateSpec where

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.Types
import           Optics
import           Test.Hspec

generateText' :: (e~ ()) =>
     Map Hash (Either e (Scheme Polymorphic))
  -> FilePath
  -> Text
  -> Either (RenameGenerateError e) (HasConstraints (Expression Generated))
generateText' = generateText

spec :: Spec
spec = do
  globals
  signatures
  variants
  it
    "Literal"
    (do shouldBe
          (second (set hasConstraintsMappingsL mempty) (generateText' mempty "" "123::Integer"))
          (Right
             (HasConstraints
                { equalities = Seq.fromList []
                , thing =
                    LiteralExpression
                      (NumberLiteral
                         (Number
                            { location = ExpressionCursor
                            , number = IntegerNumber 123
                            , typ =
                                ConstantType
                                  (TypeConstant
                                     { location = ExpressionCursor
                                     , name = IntegerTypeName
                                     })
                            }))
                , mappings = M.fromList []
                }))
        shouldBe
          (second (set hasConstraintsMappingsL mempty) (generateText' mempty "" "123.0 :: Decimal 1"))
          (Right
             (HasConstraints
                { equalities = Seq.fromList []
                , thing =
                    LiteralExpression
                      (NumberLiteral
                         (Number
                            { location = ExpressionCursor
                            , number =
                                DecimalNumber
                                  (Decimal {places = 1, integer = 1230})
                            , typ =
                                ApplyType
                                  (TypeApplication
                                     { function =
                                         ConstantType
                                           (TypeConstant
                                              { location = ExpressionCursor
                                              , name = DecimalTypeName
                                              })
                                     , argument =
                                         ConstantType
                                           TypeConstant
                                             { name = NatTypeName 1
                                             , location = ExpressionCursor
                                             }
                                     , location = ExpressionCursor
                                     , kind = TypeKind
                                     })
                            }))
                , mappings = M.fromList []
                }))
        shouldBe
          (second (set hasConstraintsMappingsL mempty) (generateText' mempty "" "0.00 ::Decimal 2"))
          (Right
             (HasConstraints
                { equalities = Seq.fromList []
                , thing =
                    LiteralExpression
                      (NumberLiteral
                         (Number
                            { location = ExpressionCursor
                            , number =
                                DecimalNumber
                                  (Decimal {places = 2, integer = 0})
                            , typ =
                                ApplyType
                                  (TypeApplication
                                     { function =
                                         ConstantType
                                           (TypeConstant
                                              { location = ExpressionCursor
                                              , name = DecimalTypeName
                                              })
                                     , argument =
                                         ConstantType
                                           TypeConstant
                                             { name = NatTypeName 2
                                             , location = ExpressionCursor
                                             }
                                     , location = ExpressionCursor
                                     , kind = TypeKind
                                     })
                            }))
                , mappings = M.fromList []
                })))
  it
    "Lambda"
    (shouldBe
       (second (set hasConstraintsMappingsL mempty) (generateText' mempty "" "x:(123::Integer)"))
       (Right
          (HasConstraints
             { equalities = Seq.fromList []
             , thing =
                 LambdaExpression
                   (Lambda
                      { location = ExpressionCursor
                      , param =
                          Param
                            { location = LambdaParamCursor
                            , name = ()
                            , typ =
                                VariableType
                                  (TypeVariable
                                     { location = LambdaParamCursor
                                     , prefix = LambdaParameterPrefix
                                     , index = 0
                                     , kind = TypeKind
                                     })
                            }
                      , body =
                          LiteralExpression
                            (NumberLiteral
                               (Number
                                  { location = LambdaBodyCursor ExpressionCursor
                                  , number = IntegerNumber 123
                                  , typ =
                                      ConstantType
                                        (TypeConstant
                                           { location =
                                               LambdaBodyCursor ExpressionCursor
                                           , name = IntegerTypeName
                                           })
                                  }))
                      , typ =
                          ApplyType
                            (TypeApplication
                               { function =
                                   ApplyType
                                     (TypeApplication
                                        { function =
                                            ConstantType
                                              (TypeConstant
                                                 { location = ExpressionCursor
                                                 , name = FunctionTypeName
                                                 })
                                        , argument =
                                            VariableType
                                              (TypeVariable
                                                 { location = LambdaParamCursor
                                                 , prefix =
                                                     LambdaParameterPrefix
                                                 , index = 0
                                                 , kind = TypeKind
                                                 })
                                        , location = ExpressionCursor
                                        , kind = FunKind TypeKind TypeKind
                                        })
                               , argument =
                                   ConstantType
                                     (TypeConstant
                                        { location =
                                            LambdaBodyCursor ExpressionCursor
                                        , name = IntegerTypeName
                                        })
                               , location = ExpressionCursor
                               , kind = TypeKind
                               })
                      })
             , mappings = M.fromList []
             })))
  it
    "Apply"
    (do shouldBe
          (second
             (set hasConstraintsMappingsL mempty)
             (generateText' mempty "" "(x:x)(123::Integer)"))
          (Right
             (HasConstraints
                { equalities =
                    Seq.fromList
                      [ EqualityConstraint
                          { type1 =
                              VariableType
                                (TypeVariable
                                   { location =
                                       ApplyFuncCursor
                                         (LambdaBodyCursor ExpressionCursor)
                                   , prefix = VariablePrefix
                                   , index = 1
                                   , kind = TypeKind
                                   })
                          , type2 =
                              VariableType
                                (TypeVariable
                                   { location =
                                       ApplyFuncCursor LambdaParamCursor
                                   , prefix = LambdaParameterPrefix
                                   , index = 0
                                   , kind = TypeKind
                                   })
                          , location =
                              ApplyFuncCursor
                                (LambdaBodyCursor ExpressionCursor)
                          }
                      , EqualityConstraint
                          { type1 =
                              ApplyType
                                (TypeApplication
                                   { function =
                                       ApplyType
                                         (TypeApplication
                                            { function =
                                                ConstantType
                                                  (TypeConstant
                                                     { location =
                                                         ApplyFuncCursor
                                                           ExpressionCursor
                                                     , name = FunctionTypeName
                                                     })
                                            , argument =
                                                VariableType
                                                  (TypeVariable
                                                     { location =
                                                         ApplyFuncCursor
                                                           LambdaParamCursor
                                                     , prefix =
                                                         LambdaParameterPrefix
                                                     , index = 0
                                                     , kind = TypeKind
                                                     })
                                            , location =
                                                ApplyFuncCursor ExpressionCursor
                                            , kind = FunKind TypeKind TypeKind
                                            })
                                   , argument =
                                       VariableType
                                         (TypeVariable
                                            { location =
                                                ApplyFuncCursor
                                                  (LambdaBodyCursor
                                                     ExpressionCursor)
                                            , prefix = VariablePrefix
                                            , index = 1
                                            , kind = TypeKind
                                            })
                                   , location = ApplyFuncCursor ExpressionCursor
                                   , kind = TypeKind
                                   })
                          , type2 =
                              ApplyType
                                (TypeApplication
                                   { function =
                                       ApplyType
                                         (TypeApplication
                                            { function =
                                                ConstantType
                                                  (TypeConstant
                                                     { location =
                                                         ExpressionCursor
                                                     , name = FunctionTypeName
                                                     })
                                            , argument =
                                                ConstantType
                                                  (TypeConstant
                                                     { location =
                                                         ApplyArgCursor
                                                           ExpressionCursor
                                                     , name = IntegerTypeName
                                                     })
                                            , location = ExpressionCursor
                                            , kind = FunKind TypeKind TypeKind
                                            })
                                   , argument =
                                       VariableType
                                         (TypeVariable
                                            { location =
                                                ApplyArgCursor ExpressionCursor
                                            , prefix = ApplyPrefix
                                            , index = 2
                                            , kind = TypeKind
                                            })
                                   , location = ApplyFuncCursor ExpressionCursor
                                   , kind = TypeKind
                                   })
                          , location = ExpressionCursor
                          }
                      ]
                , thing =
                    ApplyExpression
                      (Apply
                         { location = ExpressionCursor
                         , function =
                             LambdaExpression
                               (Lambda
                                  { location = ApplyFuncCursor ExpressionCursor
                                  , param =
                                      Param
                                        { location =
                                            ApplyFuncCursor LambdaParamCursor
                                        , name = ()
                                        , typ =
                                            VariableType
                                              (TypeVariable
                                                 { location =
                                                     ApplyFuncCursor
                                                       LambdaParamCursor
                                                 , prefix =
                                                     LambdaParameterPrefix
                                                 , index = 0
                                                 , kind = TypeKind
                                                 })
                                        }
                                  , body =
                                      VariableExpression
                                        (Variable
                                           { location =
                                               ApplyFuncCursor
                                                 (LambdaBodyCursor
                                                    ExpressionCursor)
                                           , name = DeBrujinIndex 0
                                           , typ =
                                               VariableType
                                                 (TypeVariable
                                                    { location =
                                                        ApplyFuncCursor
                                                          (LambdaBodyCursor
                                                             ExpressionCursor)
                                                    , prefix = VariablePrefix
                                                    , index = 1
                                                    , kind = TypeKind
                                                    })
                                           })
                                  , typ =
                                      ApplyType
                                        (TypeApplication
                                           { function =
                                               ApplyType
                                                 (TypeApplication
                                                    { function =
                                                        ConstantType
                                                          (TypeConstant
                                                             { location =
                                                                 ApplyFuncCursor
                                                                   ExpressionCursor
                                                             , name =
                                                                 FunctionTypeName
                                                             })
                                                    , argument =
                                                        VariableType
                                                          (TypeVariable
                                                             { location =
                                                                 ApplyFuncCursor
                                                                   LambdaParamCursor
                                                             , prefix =
                                                                 LambdaParameterPrefix
                                                             , index = 0
                                                             , kind = TypeKind
                                                             })
                                                    , location =
                                                        ApplyFuncCursor
                                                          ExpressionCursor
                                                    , kind =
                                                        FunKind
                                                          TypeKind
                                                          TypeKind
                                                    })
                                           , argument =
                                               VariableType
                                                 (TypeVariable
                                                    { location =
                                                        ApplyFuncCursor
                                                          (LambdaBodyCursor
                                                             ExpressionCursor)
                                                    , prefix = VariablePrefix
                                                    , index = 1
                                                    , kind = TypeKind
                                                    })
                                           , location =
                                               ApplyFuncCursor ExpressionCursor
                                           , kind = TypeKind
                                           })
                                  })
                         , argument =
                             LiteralExpression
                               (NumberLiteral
                                  (Number
                                     { location =
                                         ApplyArgCursor ExpressionCursor
                                     , number = IntegerNumber 123
                                     , typ =
                                         ConstantType
                                           (TypeConstant
                                              { location =
                                                  ApplyArgCursor
                                                    ExpressionCursor
                                              , name = IntegerTypeName
                                              })
                                     }))
                         , typ =
                             VariableType
                               (TypeVariable
                                  { location = ApplyArgCursor ExpressionCursor
                                  , prefix = ApplyPrefix
                                  , index = 2
                                  , kind = TypeKind
                                  })
                         })
                , mappings = M.fromList []
                })))

globals :: SpecWith ()
globals =
  describe
    "Globals"
    (describe
       "Methods"
       (do it
             "fromInteger"
             (shouldBe
                (generateText' mempty "" "@prim:from_integer")
                (Right (HasConstraints {equalities = mempty, thing = GlobalExpression (Global {location = ExpressionCursor, name = FromIntegerGlobal, scheme = GeneratedScheme (Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = ExpressionCursor, prefix = IntegerPrefix, index = 0, kind = TypeKind}) :| [], location = ExpressionCursor}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ExpressionCursor, prefix = IntegerPrefix, index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 19, name = ""}})]})))
           it
             "fromDecimal"
             (shouldBe
                (generateText' mempty "" "@prim:from_decimal")
                (Right (HasConstraints {equalities = mempty, thing = GlobalExpression (Global {location = ExpressionCursor, name = FromDecimalGlobal, scheme = GeneratedScheme (Scheme {location = ExpressionCursor, constraints = [ClassConstraint {className = FromDecimalClassName, typ = VariableType (TypeVariable {location = ExpressionCursor, prefix = NatPrefix, index = 1, kind = NatKind}) :| [VariableType (TypeVariable {location = ExpressionCursor, prefix = DecimalPrefix, index = 0, kind = TypeKind})], location = ExpressionCursor}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = DecimalTypeName}), argument = VariableType (TypeVariable {location = ExpressionCursor, prefix = NatPrefix, index = 1, kind = NatKind}), location = BuiltIn, kind = TypeKind}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ExpressionCursor, prefix = DecimalPrefix, index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 19, name = ""}})]})))))

signatures :: SpecWith ()
signatures =
  describe
    "Signatures"
    (it
       "fromInteger (123 :: Integer) :: Decimal 2"
       (shouldBe
          (generateText' mempty "" "@prim:from_integer(123 :: Integer) :: Decimal 2")
          (Right (HasConstraints {equalities = Seq.fromList [EqualityConstraint {type1 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor ExpressionCursor, prefix = IntegerPrefix, index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind}), type2 = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = ExpressionCursor, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName}), location = ExpressionCursor, kind = FunKind TypeKind TypeKind}), argument = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = NatTypeName 2}), location = SignatureCursor TypeCursor, kind = TypeKind}), location = ApplyFuncCursor ExpressionCursor, kind = TypeKind}), location = ExpressionCursor}], thing = ApplyExpression (Apply {location = ExpressionCursor, function = GlobalExpression (Global {location = ApplyFuncCursor ExpressionCursor, name = FromIntegerGlobal, scheme = GeneratedScheme (Scheme {location = ApplyFuncCursor ExpressionCursor, constraints = [ClassConstraint {className = FromIntegerClassName, typ = VariableType (TypeVariable {location = ApplyFuncCursor ExpressionCursor, prefix = IntegerPrefix, index = 0, kind = TypeKind}) :| [], location = ApplyFuncCursor ExpressionCursor}], typ = ApplyType (TypeApplication {function = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = BuiltIn, name = FunctionTypeName}), argument = ConstantType (TypeConstant {location = BuiltIn, name = IntegerTypeName}), location = BuiltIn, kind = FunKind TypeKind TypeKind}), argument = VariableType (TypeVariable {location = ApplyFuncCursor ExpressionCursor, prefix = IntegerPrefix, index = 0, kind = TypeKind}), location = BuiltIn, kind = TypeKind})})}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = ConstantType (TypeConstant {location = ApplyArgCursor ExpressionCursor, name = IntegerTypeName})})), typ = ApplyType (TypeApplication {function = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = DecimalTypeName}), argument = ConstantType (TypeConstant {location = SignatureCursor (TypeApplyCursor TypeCursor), name = NatTypeName 2}), location = SignatureCursor TypeCursor, kind = TypeKind})}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 20, name = ""}, end = SourcePos {line = 1, column = 23, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 19, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 20, name = ""}, end = SourcePos {line = 1, column = 23, name = ""}}),(ApplyArgCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 27, name = ""}, end = SourcePos {line = 1, column = 34, name = ""}}),(SignatureCursor TypeCursor,SourceLocation {start = SourcePos {line = 1, column = 39, name = ""}, end = SourcePos {line = 1, column = 48, name = ""}}),(SignatureCursor (TypeApplyCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 47, name = ""}, end = SourcePos {line = 1, column = 48, name = ""}})]}))))

variants :: SpecWith ()
variants =
  describe
    "Variants"
    (it
       "#true"
       (shouldBe
          (generateText' mempty "" "#true")
          (Right
             (HasConstraints
                { equalities = mempty
                , thing =
                    VariantExpression
                      (Variant
                         { location = ExpressionCursor
                         , typ =
                             VariantType
                               (RowType
                                  (TypeRow
                                     { location = ExpressionCursor
                                     , typeVariable =
                                         Just
                                           (TypeVariable
                                              { location = ExpressionCursor
                                              , prefix = VariantRowVarPrefix
                                              , index = 0
                                              , kind = RowKind
                                              })
                                     , fields =
                                         [ Field
                                             { location = ExpressionCursor
                                             , name =
                                                 FieldName
                                                   {unFieldName = "true"}
                                             , typ =
                                                 RowType
                                                   (TypeRow
                                                      { location =
                                                          ExpressionCursor
                                                      , typeVariable = Nothing
                                                      , fields = []
                                                      })
                                             }
                                         ]
                                     }))
                         , tag = TagName {unTagName = "true"}
                         , argument = Nothing
                         })
                , mappings =
                    M.fromList
                      [ ( ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 6, name = ""}
                            })
                      ]
                }))))
