{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module GeneraliserSpec where

import qualified Data.Map.Strict as M
import           Inflex.Generaliser
import           Inflex.Instances ()
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "Fine-grained" fineGrained
  describe "Coarse-grained" coarseGrained

coarseGrained :: Spec
coarseGrained = do
  it
    "\\x->123"
    (shouldBe
       (generaliseText "" "\\x->123")
       (Right
          (IsGeneralised
             { thing =
                 LambdaExpression
                   (Lambda
                      { location = ExpressionCursor
                      , param =
                          Param
                            { location = LambdaParamCursor
                            , name = ()
                            , typ =
                                PolyType
                                  (TypeVariable
                                     { location = ()
                                     , prefix = ()
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
                                      PolyType
                                        (TypeVariable
                                           { location = ()
                                           , prefix = ()
                                           , index = 1
                                           , kind = TypeKind
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
                                            PolyType
                                              (TypeVariable
                                                 { location = ()
                                                 , prefix = ()
                                                 , index = 0
                                                 , kind = TypeKind
                                                 })
                                        , location = ExpressionCursor
                                        , kind = FunKind TypeKind TypeKind
                                        })
                               , argument =
                                   PolyType
                                     (TypeVariable
                                        { location = ()
                                        , prefix = ()
                                        , index = 1
                                        , kind = TypeKind
                                        })
                               , location = ExpressionCursor
                               , kind = TypeKind
                               })
                      })
             , polytype =
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
                                        { location = ()
                                        , prefix = ()
                                        , index = 0
                                        , kind = TypeKind
                                        })
                               , location = ExpressionCursor
                               , kind = FunKind TypeKind TypeKind
                               })
                      , argument =
                          VariableType
                            (TypeVariable
                               { location = ()
                               , prefix = ()
                               , index = 1
                               , kind = TypeKind
                               })
                      , location = ExpressionCursor
                      , kind = TypeKind
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
    "(\\x->123)(\\x->x)"
    (shouldBe
       (generaliseText "" "(\\x->123)(\\x->x)")
       (Right
          (IsGeneralised
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
                                                                    ApplyArgCursor
                                                                      ExpressionCursor
                                                                , name =
                                                                    FunctionTypeName
                                                                })
                                                       , argument =
                                                           VariableType
                                                             (TypeVariable
                                                                { location =
                                                                    ApplyArgCursor
                                                                      LambdaParamCursor
                                                                , prefix =
                                                                    LambdaParameterPrefix
                                                                , index = 2
                                                                , kind =
                                                                    TypeKind
                                                                })
                                                       , location =
                                                           ApplyArgCursor
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
                                                           ApplyArgCursor
                                                             LambdaParamCursor
                                                       , prefix =
                                                           LambdaParameterPrefix
                                                       , index = 2
                                                       , kind = TypeKind
                                                       })
                                              , location =
                                                  ApplyArgCursor
                                                    ExpressionCursor
                                              , kind = TypeKind
                                              })
                                     }
                               , body =
                                   LiteralExpression
                                     (NumberLiteral
                                        (Number
                                           { location =
                                               ApplyFuncCursor
                                                 (LambdaBodyCursor
                                                    ExpressionCursor)
                                           , number = IntegerNumber 123
                                           , typ =
                                               PolyType
                                                 (TypeVariable
                                                    { location = ()
                                                    , prefix = ()
                                                    , index = 0
                                                    , kind = TypeKind
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
                                                          { location =
                                                              ApplyFuncCursor
                                                                ExpressionCursor
                                                          , name =
                                                              FunctionTypeName
                                                          })
                                                 , argument =
                                                     ApplyType
                                                       (TypeApplication
                                                          { function =
                                                              ApplyType
                                                                (TypeApplication
                                                                   { function =
                                                                       ConstantType
                                                                         (TypeConstant
                                                                            { location =
                                                                                ApplyArgCursor
                                                                                  ExpressionCursor
                                                                            , name =
                                                                                FunctionTypeName
                                                                            })
                                                                   , argument =
                                                                       VariableType
                                                                         (TypeVariable
                                                                            { location =
                                                                                ApplyArgCursor
                                                                                  LambdaParamCursor
                                                                            , prefix =
                                                                                LambdaParameterPrefix
                                                                            , index =
                                                                                2
                                                                            , kind =
                                                                                TypeKind
                                                                            })
                                                                   , location =
                                                                       ApplyArgCursor
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
                                                                       ApplyArgCursor
                                                                         LambdaParamCursor
                                                                   , prefix =
                                                                       LambdaParameterPrefix
                                                                   , index = 2
                                                                   , kind =
                                                                       TypeKind
                                                                   })
                                                          , location =
                                                              ApplyArgCursor
                                                                ExpressionCursor
                                                          , kind = TypeKind
                                                          })
                                                 , location =
                                                     ApplyFuncCursor
                                                       ExpressionCursor
                                                 , kind =
                                                     FunKind TypeKind TypeKind
                                                 })
                                        , argument =
                                            PolyType
                                              (TypeVariable
                                                 { location = ()
                                                 , prefix = ()
                                                 , index = 0
                                                 , kind = TypeKind
                                                 })
                                        , location =
                                            ApplyFuncCursor ExpressionCursor
                                        , kind = TypeKind
                                        })
                               })
                      , argument =
                          LambdaExpression
                            (Lambda
                               { location = ApplyArgCursor ExpressionCursor
                               , param =
                                   Param
                                     { location =
                                         ApplyArgCursor LambdaParamCursor
                                     , name = ()
                                     , typ =
                                         VariableType
                                           (TypeVariable
                                              { location =
                                                  ApplyArgCursor
                                                    LambdaParamCursor
                                              , prefix = LambdaParameterPrefix
                                              , index = 2
                                              , kind = TypeKind
                                              })
                                     }
                               , body =
                                   VariableExpression
                                     (Variable
                                        { location =
                                            ApplyArgCursor
                                              (LambdaBodyCursor ExpressionCursor)
                                        , name = DeBrujinIndex 0
                                        , typ =
                                            VariableType
                                              (TypeVariable
                                                 { location =
                                                     ApplyArgCursor
                                                       LambdaParamCursor
                                                 , prefix =
                                                     LambdaParameterPrefix
                                                 , index = 2
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
                                                              ApplyArgCursor
                                                                ExpressionCursor
                                                          , name =
                                                              FunctionTypeName
                                                          })
                                                 , argument =
                                                     VariableType
                                                       (TypeVariable
                                                          { location =
                                                              ApplyArgCursor
                                                                LambdaParamCursor
                                                          , prefix =
                                                              LambdaParameterPrefix
                                                          , index = 2
                                                          , kind = TypeKind
                                                          })
                                                 , location =
                                                     ApplyArgCursor
                                                       ExpressionCursor
                                                 , kind =
                                                     FunKind TypeKind TypeKind
                                                 })
                                        , argument =
                                            VariableType
                                              (TypeVariable
                                                 { location =
                                                     ApplyArgCursor
                                                       LambdaParamCursor
                                                 , prefix =
                                                     LambdaParameterPrefix
                                                 , index = 2
                                                 , kind = TypeKind
                                                 })
                                        , location =
                                            ApplyArgCursor ExpressionCursor
                                        , kind = TypeKind
                                        })
                               })
                      , typ =
                          PolyType
                            (TypeVariable
                               { location = ()
                               , prefix = ()
                               , index = 0
                               , kind = TypeKind
                               })
                      })
             , polytype =
                 VariableType
                   (TypeVariable
                      {location = (), prefix = (), index = 0, kind = TypeKind})
             , mappings =
                 M.fromList
                   [ ( ExpressionCursor
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 2, name = ""}
                         , end = SourcePos {line = 1, column = 9, name = ""}
                         })
                   , ( ApplyFuncCursor ExpressionCursor
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 2, name = ""}
                         , end = SourcePos {line = 1, column = 9, name = ""}
                         })
                   , ( ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 6, name = ""}
                         , end = SourcePos {line = 1, column = 9, name = ""}
                         })
                   , ( ApplyFuncCursor LambdaParamCursor
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 3, name = ""}
                         , end = SourcePos {line = 1, column = 4, name = ""}
                         })
                   , ( ApplyArgCursor ExpressionCursor
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 11, name = ""}
                         , end = SourcePos {line = 1, column = 16, name = ""}
                         })
                   , ( ApplyArgCursor (LambdaBodyCursor ExpressionCursor)
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 15, name = ""}
                         , end = SourcePos {line = 1, column = 16, name = ""}
                         })
                   , ( ApplyArgCursor LambdaParamCursor
                     , SourceLocation
                         { start = SourcePos {line = 1, column = 12, name = ""}
                         , end = SourcePos {line = 1, column = 13, name = ""}
                         })
                   ]
             })))

fineGrained :: Spec
fineGrained =
  it
    "Polymorphise a type"
    (shouldBe
       (toPolymorphic
          (ApplyType
             (TypeApplication
                { function =
                    ApplyType
                      (TypeApplication
                         { function =
                             ConstantType
                               (TypeConstant
                                  { location = ApplyFuncCursor ExpressionCursor
                                  , name = FunctionTypeName
                                  })
                         , argument =
                             VariableType
                               (TypeVariable
                                  { location = ApplyArgCursor ExpressionCursor
                                  , prefix = ApplyPrefix
                                  , index = 2
                                  , kind = TypeKind
                                  })
                         , location = ApplyFuncCursor ExpressionCursor
                         , kind = FunKind TypeKind TypeKind
                         })
                , argument =
                    VariableType
                      (TypeVariable
                         { location = ApplyArgCursor ExpressionCursor
                         , prefix = ApplyPrefix
                         , index = 3
                         , kind = TypeKind
                         })
                , location = ApplyFuncCursor ExpressionCursor
                , kind = TypeKind
                })))
       ( ApplyType
           (TypeApplication
              { function =
                  ApplyType
                    (TypeApplication
                       { function =
                           ConstantType
                             (TypeConstant
                                { location = ApplyFuncCursor ExpressionCursor
                                , name = FunctionTypeName
                                })
                       , argument =
                           VariableType
                             (TypeVariable
                                { location = ()
                                , prefix = ()
                                , index = 0
                                , kind = TypeKind
                                })
                       , location = ApplyFuncCursor ExpressionCursor
                       , kind = FunKind TypeKind TypeKind
                       })
              , argument =
                  VariableType
                    (TypeVariable
                       {location = (), prefix = (), index = 1, kind = TypeKind})
              , location = ApplyFuncCursor ExpressionCursor
              , kind = TypeKind
              })
       , M.fromList
           [ ( TypeVariable
                 { location = ApplyArgCursor ExpressionCursor
                 , prefix = ApplyPrefix
                 , index = 2
                 , kind = TypeKind
                 }
             , TypeVariable
                 {location = (), prefix = (), index = 0, kind = TypeKind})
           , ( TypeVariable
                 { location = ApplyArgCursor ExpressionCursor
                 , prefix = ApplyPrefix
                 , index = 3
                 , kind = TypeKind
                 }
             , TypeVariable
                 {location = (), prefix = (), index = 1, kind = TypeKind})
           ]))
