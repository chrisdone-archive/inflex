{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Tests for generation of type constraints.

module GenerateSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (generateText "" "123")
       (Right
          (HasConstraints
             { classes =
                 Seq.fromList
                   [ ClassConstraint
                       { className = FromIntegerClassName
                       , types =
                           VariableType
                             TypeVariable
                               { prefix = IntegeryPrefix
                               , index = 0
                               , location = ExpressionCursor
                               } :|
                           []
                       , location = ExpressionCursor
                       }
                   ]
             , thing =
                 LiteralExpression
                   (IntegerLiteral
                      (Integery
                         { location = ExpressionCursor
                         , integer = 123
                         , typ =
                             VariableType
                               TypeVariable
                                 { prefix = IntegeryPrefix
                                 , index = 0
                                 , location = ExpressionCursor
                                 }
                         }))
             , mappings =
                 M.fromList
                   [ ( ExpressionCursor
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 1, name = ""}
                         , end = SourcePos {line = 1, column = 4, name = ""}
                         })
                   ]
             , equalities = mempty
             })))
  it
    "Lambda"
    (shouldBe
       (generateText "" "\\x->123")
       (Right
          (HasConstraints
             { classes =
                 Seq.fromList
                   [ ClassConstraint
                       { className = FromIntegerClassName
                       , types =
                           VariableType
                             (TypeVariable
                                { location = LambdaBodyCursor ExpressionCursor
                                , prefix = IntegeryPrefix
                                , index = 1
                                }) :|
                           []
                       , location = LambdaBodyCursor ExpressionCursor
                       }
                   ]
             , equalities = mempty
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
                                     })
                            }
                      , body =
                          LiteralExpression
                            (IntegerLiteral
                               (Integery
                                  { location = LambdaBodyCursor ExpressionCursor
                                  , integer = 123
                                  , typ =
                                      VariableType
                                        (TypeVariable
                                           { location =
                                               LambdaBodyCursor ExpressionCursor
                                           , prefix = IntegeryPrefix
                                           , index = 1
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
                                                 })
                                        , location = ExpressionCursor
                                        })
                               , argument =
                                   VariableType
                                     (TypeVariable
                                        { location =
                                            LambdaBodyCursor ExpressionCursor
                                        , prefix = IntegeryPrefix
                                        , index = 1
                                        })
                               , location = ExpressionCursor
                               })
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
             })))
  it
    "Apply"
    (do shouldBe
          (generateText "" "(\\x->x)123")
          (Right
             (HasConstraints
                { classes =
                    Seq.fromList
                      [ ClassConstraint
                          { className = FromIntegerClassName
                          , types =
                              VariableType
                                (TypeVariable
                                   { location = ApplyArgCursor ExpressionCursor
                                   , prefix = IntegeryPrefix
                                   , index = 2
                                   }) :|
                              []
                          , location = ApplyArgCursor ExpressionCursor
                          }
                      ]
                , equalities =
                    Seq.fromList
                      [ EqualityConstraint
                          { type1 =
                              VariableType
                                (TypeVariable
                                   { location =
                                       ApplyFuncCursor
                                         (LambdaBodyCursor ExpressionCursor)
                                   , prefix = LambdaParameterPrefix
                                   , index = 1
                                   })
                          , type2 =
                              VariableType
                                (TypeVariable
                                   { location =
                                       ApplyFuncCursor LambdaParamCursor
                                   , prefix = LambdaParameterPrefix
                                   , index = 0
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
                                                     })
                                            , location =
                                                ApplyFuncCursor ExpressionCursor
                                            })
                                   , argument =
                                       VariableType
                                         (TypeVariable
                                            { location =
                                                ApplyFuncCursor
                                                  (LambdaBodyCursor
                                                     ExpressionCursor)
                                            , prefix = LambdaParameterPrefix
                                            , index = 1
                                            })
                                   , location = ApplyFuncCursor ExpressionCursor
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
                                                VariableType
                                                  (TypeVariable
                                                     { location =
                                                         ApplyArgCursor
                                                           ExpressionCursor
                                                     , prefix = IntegeryPrefix
                                                     , index = 2
                                                     })
                                            , location = ExpressionCursor
                                            })
                                   , argument =
                                       VariableType
                                         (TypeVariable
                                            { location =
                                                ApplyArgCursor ExpressionCursor
                                            , prefix = ApplyPrefix
                                            , index = 3
                                            })
                                   , location = ApplyFuncCursor ExpressionCursor
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
                                                    , prefix =
                                                        LambdaParameterPrefix
                                                    , index = 1
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
                                                             })
                                                    , location =
                                                        ApplyFuncCursor
                                                          ExpressionCursor
                                                    })
                                           , argument =
                                               VariableType
                                                 (TypeVariable
                                                    { location =
                                                        ApplyFuncCursor
                                                          (LambdaBodyCursor
                                                             ExpressionCursor)
                                                    , prefix =
                                                        LambdaParameterPrefix
                                                    , index = 1
                                                    })
                                           , location =
                                               ApplyFuncCursor ExpressionCursor
                                           })
                                  })
                         , argument =
                             LiteralExpression
                               (IntegerLiteral
                                  (Integery
                                     { location =
                                         ApplyArgCursor ExpressionCursor
                                     , integer = 123
                                     , typ =
                                         VariableType
                                           (TypeVariable
                                              { location =
                                                  ApplyArgCursor
                                                    ExpressionCursor
                                              , prefix = IntegeryPrefix
                                              , index = 2
                                              })
                                     }))
                         , typ =
                             VariableType
                               (TypeVariable
                                  { location = ApplyArgCursor ExpressionCursor
                                  , prefix = ApplyPrefix
                                  , index = 3
                                  })
                         })
                , mappings =
                    M.fromList
                      [ ( ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 7, name = ""}
                            })
                      , ( ApplyFuncCursor ExpressionCursor
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 7, name = ""}
                            })
                      , ( ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)
                        , SourceLocation
                            { start =
                                SourcePos {line = 1, column = 6, name = ""}
                            , end = SourcePos {line = 1, column = 7, name = ""}
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
                                SourcePos {line = 1, column = 8, name = ""}
                            , end = SourcePos {line = 1, column = 11, name = ""}
                            })
                      ]
                })))
