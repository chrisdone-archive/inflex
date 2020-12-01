{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module RenameSpec where

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Inflex.Instances ()
import           Inflex.Renamer
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (do shouldBe
          (renameText "" "123::Integer")
          (Right
             (IsRenamed
                { unresolvedGlobals = mempty
                , thing =
                    LiteralExpression
                      (NumberLiteral
                         (Number
                            { location = ExpressionCursor
                            , number = IntegerNumber 123
                            , typ =
                                Just
                                  (ConstantType
                                     (TypeConstant
                                        { location = SignatureCursor TypeCursor
                                        , name = IntegerTypeName
                                        }))
                            }))
                , mappings =
                    M.fromList
                      [ ( ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
                            , end = SourcePos {line = 1, column = 4, name = ""}
                            })
                      , ( SignatureCursor TypeCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 6, name = ""}
                            , end = SourcePos {line = 1, column = 13, name = ""}
                            })
                      ]
                }))
        shouldBe
          (renameText "" "123.2 :: Integer")
          (Right
             (IsRenamed
                { unresolvedGlobals = mempty
                , thing =
                    ApplyExpression
                      (Apply
                         { location = BuiltIn
                         , function =
                             GlobalExpression
                               (Global
                                  { location = BuiltIn
                                  , name = ExactGlobalRef FromDecimalGlobal
                                  , scheme = RenamedScheme
                                  })
                         , argument =
                             LiteralExpression
                               (NumberLiteral
                                  (Number
                                     { location = ExpressionCursor
                                     , number =
                                         DecimalNumber
                                           (Decimal {places = 1, integer = 1232})
                                     , typ =
                                         Just
                                           (ConstantType
                                              (TypeConstant
                                                 { location =
                                                     SignatureCursor TypeCursor
                                                 , name = IntegerTypeName
                                                 }))
                                     }))
                         , typ =
                             Just
                               (ConstantType
                                  (TypeConstant
                                     { location = SignatureCursor TypeCursor
                                     , name = IntegerTypeName
                                     }))
                         })
                , mappings =
                    M.fromList
                      [ ( ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
                            , end = SourcePos {line = 1, column = 6, name = ""}
                            })
                      , ( SignatureCursor TypeCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 10, name = ""}
                            , end = SourcePos {line = 1, column = 17, name = ""}
                            })
                      ]
                })))
  describe
    "Globals"
    (do it
          "missing"
          (shouldBe
             (renameText "" "missing")
             (Right
                (IsRenamed
                   { thing =
                       GlobalExpression
                         (Global
                            { location = ExpressionCursor
                            , name = UnresolvedGlobal "missing"
                            , scheme = RenamedScheme
                            })
                   , mappings =
                       M.fromList
                         [ ( ExpressionCursor
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 8, name = ""}
                               })
                         ]
                   , unresolvedGlobals = Set.fromList ["missing"]
                   })))
        it
          "fromInteger"
          (shouldBe
             (renameText "" "fromInteger")
             (Right
                (IsRenamed
                   { unresolvedGlobals = mempty
                   , thing =
                       GlobalExpression
                         (Global
                            { location = ExpressionCursor
                            , name = ResolvedGlobalRef "fromInteger" FromIntegerGlobal
                            , scheme = RenamedScheme
                            })
                   , mappings =
                       M.fromList
                         [ ( ExpressionCursor
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 12, name = ""}
                               })
                         ]
                   })))
        it
          "fromDecimal"
          (shouldBe
             (renameText "" "fromDecimal")
             (Right
                (IsRenamed
                   { unresolvedGlobals = mempty
                   , thing =
                       GlobalExpression
                         (Global
                            { location = ExpressionCursor
                            , name = ResolvedGlobalRef "fromDecimal" FromDecimalGlobal
                            , scheme = RenamedScheme
                            })
                   , mappings =
                       M.fromList
                         [ ( ExpressionCursor
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 12, name = ""}
                               })
                         ]
                   }))))
  it
    "Lambda"
    (shouldBe
       (renameText "" "x:(123::Integer)")
       (Right (IsRenamed {thing = LambdaExpression (Lambda {location = ExpressionCursor, param = Param {location = LambdaParamCursor, name = (), typ = Nothing}, body = LiteralExpression (NumberLiteral (Number {location = LambdaBodyCursor ExpressionCursor, number = IntegerNumber 123, typ = Just (ConstantType (TypeConstant {location = LambdaBodyCursor (SignatureCursor TypeCursor), name = IntegerTypeName}))})), typ = Nothing}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}),(LambdaBodyCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 4, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}),(LambdaBodyCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 9, name = ""}, end = SourcePos {line = 1, column = 16, name = ""}}),(LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 2, name = ""}})], unresolvedGlobals = mempty})))
  it
    "Apply: debrujin 0 and 0"
    (do shouldBe
          (renameText "" "(x:(y:y)(x))(123::Integer)")
          (Right (IsRenamed {thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = Nothing}, body = ApplyExpression (Apply {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), function = LambdaExpression (Lambda {location = ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), param = Param {location = ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor LambdaParamCursor)), name = (), typ = Nothing}, body = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor (LambdaBodyCursor ExpressionCursor))), name = DeBrujinIndex (DeBrujinNesting 0), typ = Nothing}), typ = Nothing}), argument = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor (ApplyArgCursor ExpressionCursor)), name = DeBrujinIndex (DeBrujinNesting 0), typ = Nothing}), typ = Nothing}), typ = Nothing}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = Just (ConstantType (TypeConstant {location = ApplyArgCursor (SignatureCursor TypeCursor), name = IntegerTypeName}))})), typ = Nothing}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 11, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 10, name = ""}, end = SourcePos {line = 1, column = 11, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)),SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor (LambdaBodyCursor ExpressionCursor))),SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor LambdaParamCursor)),SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyArgCursor ExpressionCursor)),SourceLocation {start = SourcePos {line = 1, column = 10, name = ""}, end = SourcePos {line = 1, column = 11, name = ""}}),(ApplyFuncCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyArgCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 19, name = ""}, end = SourcePos {line = 1, column = 26, name = ""}})], unresolvedGlobals = mempty})))
  it
    "Apply: debrujin 0 and 1"
    (do shouldBe
          (renameText "" "(x:(y:x)(x))(123::Integer)")
          (Right (IsRenamed {thing = ApplyExpression (Apply {location = ExpressionCursor, function = LambdaExpression (Lambda {location = ApplyFuncCursor ExpressionCursor, param = Param {location = ApplyFuncCursor LambdaParamCursor, name = (), typ = Nothing}, body = ApplyExpression (Apply {location = ApplyFuncCursor (LambdaBodyCursor ExpressionCursor), function = LambdaExpression (Lambda {location = ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)), param = Param {location = ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor LambdaParamCursor)), name = (), typ = Nothing}, body = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor (LambdaBodyCursor ExpressionCursor))), name = DeBrujinIndex (DeBrujinNesting 1), typ = Nothing}), typ = Nothing}), argument = VariableExpression (Variable {location = ApplyFuncCursor (LambdaBodyCursor (ApplyArgCursor ExpressionCursor)), name = DeBrujinIndex (DeBrujinNesting 0), typ = Nothing}), typ = Nothing}), typ = Nothing}), argument = LiteralExpression (NumberLiteral (Number {location = ApplyArgCursor ExpressionCursor, number = IntegerNumber 123, typ = Just (ConstantType (TypeConstant {location = ApplyArgCursor (SignatureCursor TypeCursor), name = IntegerTypeName}))})), typ = Nothing}), mappings = M.fromList [(ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyFuncCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 11, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor ExpressionCursor),SourceLocation {start = SourcePos {line = 1, column = 10, name = ""}, end = SourcePos {line = 1, column = 11, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor)),SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor (LambdaBodyCursor ExpressionCursor))),SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyFuncCursor LambdaParamCursor)),SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}}),(ApplyFuncCursor (LambdaBodyCursor (ApplyArgCursor ExpressionCursor)),SourceLocation {start = SourcePos {line = 1, column = 10, name = ""}, end = SourcePos {line = 1, column = 11, name = ""}}),(ApplyFuncCursor LambdaParamCursor,SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}}),(ApplyArgCursor ExpressionCursor,SourceLocation {start = SourcePos {line = 1, column = 14, name = ""}, end = SourcePos {line = 1, column = 17, name = ""}}),(ApplyArgCursor (SignatureCursor TypeCursor),SourceLocation {start = SourcePos {line = 1, column = 19, name = ""}, end = SourcePos {line = 1, column = 26, name = ""}})], unresolvedGlobals = mempty})))
