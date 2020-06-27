{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module RenameSpec where

import qualified Data.Map.Strict as M
import           Inflex.Instances ()
import           Inflex.Renamer
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (renameText "" "123")
       (Right
          IsRenamed
            { thing =
                LiteralExpression
                  (NumberLiteral
                     (Number
                        { location = ExpressionCursor
                        , number = IntegerNumber 123
                        , typ = Nothing
                        }))
            , mappings =
                M.fromList
                  [ ( ExpressionCursor
                    , SourceLocation
                        { start = SourcePos {line = 1, column = 1, name = ""}
                        , end = SourcePos {line = 1, column = 4, name = ""}
                        })
                  ]
            }))
  describe
    "Globals"
    (do it
          "fromInteger"
          (shouldBe
             (renameText "" "fromInteger")
             (Right
                (IsRenamed
                   { thing =
                       GlobalExpression
                         (Global
                            { location = ExpressionCursor
                            , name = FromIntegerGlobal
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
                   { thing =
                       GlobalExpression
                         (Global
                            { location = ExpressionCursor
                            , name = FromDecimalGlobal
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
       (renameText "" "\\x->123")
       (Right
          IsRenamed
            { thing =
                LambdaExpression
                  (Lambda
                     { location = ExpressionCursor
                     , param =
                         Param
                           {location = LambdaParamCursor, name = (), typ = Nothing}
                     , body =
                         LiteralExpression
                           (NumberLiteral
                              (Number
                                 { location = LambdaBodyCursor ExpressionCursor
                                 , number = IntegerNumber 123
                                 , typ = Nothing
                                 }))
                     , typ = Nothing
                     })
            , mappings =
                M.fromList
                  [ ( ExpressionCursor
                    , SourceLocation
                        { start = SourcePos {line = 1, column = 1, name = ""}
                        , end = SourcePos {line = 1, column = 8, name = ""}
                        })
                  , ( LambdaBodyCursor ExpressionCursor
                    , SourceLocation
                        { start = SourcePos {line = 1, column = 5, name = ""}
                        , end = SourcePos {line = 1, column = 8, name = ""}
                        })
                  , ( LambdaParamCursor
                    , SourceLocation
                        { start = SourcePos {line = 1, column = 2, name = ""}
                        , end = SourcePos {line = 1, column = 3, name = ""}
                        })
                  ]
            }))
  it
    "Apply: debrujin 0 and 0"
    (do shouldBe
          (renameText "" "(\\x->(\\y->y)x)123")
          (Right
             (IsRenamed
                { thing =
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
                                        , typ = Nothing
                                        }
                                  , body =
                                      ApplyExpression
                                        (Apply
                                           { location =
                                               ApplyFuncCursor
                                                 (LambdaBodyCursor
                                                    ExpressionCursor)
                                           , function =
                                               LambdaExpression
                                                 (Lambda
                                                    { location =
                                                        ApplyFuncCursor
                                                          (LambdaBodyCursor
                                                             (ApplyFuncCursor
                                                                ExpressionCursor))
                                                    , param =
                                                        Param
                                                          { location =
                                                              ApplyFuncCursor
                                                                (LambdaBodyCursor
                                                                   (ApplyFuncCursor
                                                                      LambdaParamCursor))
                                                          , name = ()
                                                          , typ = Nothing
                                                          }
                                                    , body =
                                                        VariableExpression
                                                          (Variable
                                                             { location =
                                                                 ApplyFuncCursor
                                                                   (LambdaBodyCursor
                                                                      (ApplyFuncCursor
                                                                         (LambdaBodyCursor
                                                                            ExpressionCursor)))
                                                             , name =
                                                                 DeBrujinIndex 0
                                                             , typ = Nothing
                                                             })
                                                    , typ = Nothing
                                                    })
                                           , argument =
                                               VariableExpression
                                                 (Variable
                                                    { location =
                                                        ApplyFuncCursor
                                                          (LambdaBodyCursor
                                                             (ApplyArgCursor
                                                                ExpressionCursor))
                                                    , name = DeBrujinIndex 0
                                                    , typ = Nothing
                                                    })
                                           , typ = Nothing
                                           })
                                  , typ = Nothing
                                  })
                         , argument =
                             LiteralExpression
                               (NumberLiteral
                                  (Number
                                     { location =
                                         ApplyArgCursor ExpressionCursor
                                     , number = IntegerNumber 123
                                     , typ = Nothing
                                     }))
                         , typ = Nothing
                         })
                , mappings =
                    M.fromList
                      [ ( ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 18, name = ""}
                            })
                      , ( ApplyFuncCursor ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 14, name = ""}
                            })
                      , ( ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 7, name = ""}
                            , end = SourcePos {line = 1, column = 14, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 7, name = ""}
                            , end = SourcePos {line = 1, column = 12, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor
                               (ApplyFuncCursor
                                  (LambdaBodyCursor ExpressionCursor)))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 11, name = ""}
                            , end = SourcePos {line = 1, column = 12, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor
                               (ApplyFuncCursor LambdaParamCursor))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 8, name = ""}
                            , end = SourcePos {line = 1, column = 9, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor (ApplyArgCursor ExpressionCursor))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 13, name = ""}
                            , end = SourcePos {line = 1, column = 14, name = ""}
                            })
                      , ( ApplyFuncCursor LambdaParamCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 3, name = ""}
                            , end = SourcePos {line = 1, column = 4, name = ""}
                            })
                      , ( ApplyArgCursor ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 15, name = ""}
                            , end = SourcePos {line = 1, column = 18, name = ""}
                            })
                      ]
                })))
  it
    "Apply: debrujin 0 and 1"
    (do shouldBe
          (renameText "" "(\\x->(\\y->x)x)123")
          (Right
             (IsRenamed
                { thing =
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
                                        , typ = Nothing
                                        }
                                  , body =
                                      ApplyExpression
                                        (Apply
                                           { location =
                                               ApplyFuncCursor
                                                 (LambdaBodyCursor
                                                    ExpressionCursor)
                                           , function =
                                               LambdaExpression
                                                 (Lambda
                                                    { location =
                                                        ApplyFuncCursor
                                                          (LambdaBodyCursor
                                                             (ApplyFuncCursor
                                                                ExpressionCursor))
                                                    , param =
                                                        Param
                                                          { location =
                                                              ApplyFuncCursor
                                                                (LambdaBodyCursor
                                                                   (ApplyFuncCursor
                                                                      LambdaParamCursor))
                                                          , name = ()
                                                          , typ = Nothing
                                                          }
                                                    , body =
                                                        VariableExpression
                                                          (Variable
                                                             { location =
                                                                 ApplyFuncCursor
                                                                   (LambdaBodyCursor
                                                                      (ApplyFuncCursor
                                                                         (LambdaBodyCursor
                                                                            ExpressionCursor)))
                                                             , name =
                                                                 DeBrujinIndex 1
                                                             , typ = Nothing
                                                             })
                                                    , typ = Nothing
                                                    })
                                           , argument =
                                               VariableExpression
                                                 (Variable
                                                    { location =
                                                        ApplyFuncCursor
                                                          (LambdaBodyCursor
                                                             (ApplyArgCursor
                                                                ExpressionCursor))
                                                    , name = DeBrujinIndex 0
                                                    , typ = Nothing
                                                    })
                                           , typ = Nothing
                                           })
                                  , typ = Nothing
                                  })
                         , argument =
                             LiteralExpression
                               (NumberLiteral
                                  (Number
                                     { location =
                                         ApplyArgCursor ExpressionCursor
                                     , number = IntegerNumber 123
                                     , typ = Nothing
                                     }))
                         , typ = Nothing
                         })
                , mappings =
                    M.fromList
                      [ ( ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 18, name = ""}
                            })
                      , ( ApplyFuncCursor ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 14, name = ""}
                            })
                      , ( ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 7, name = ""}
                            , end = SourcePos {line = 1, column = 14, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor (ApplyFuncCursor ExpressionCursor))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 7, name = ""}
                            , end = SourcePos {line = 1, column = 12, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor
                               (ApplyFuncCursor
                                  (LambdaBodyCursor ExpressionCursor)))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 11, name = ""}
                            , end = SourcePos {line = 1, column = 12, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor
                               (ApplyFuncCursor LambdaParamCursor))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 8, name = ""}
                            , end = SourcePos {line = 1, column = 9, name = ""}
                            })
                      , ( ApplyFuncCursor
                            (LambdaBodyCursor (ApplyArgCursor ExpressionCursor))
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 13, name = ""}
                            , end = SourcePos {line = 1, column = 14, name = ""}
                            })
                      , ( ApplyFuncCursor LambdaParamCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 3, name = ""}
                            , end = SourcePos {line = 1, column = 4, name = ""}
                            })
                      , ( ApplyArgCursor ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 15, name = ""}
                            , end = SourcePos {line = 1, column = 18, name = ""}
                            })
                      ]
                })))
