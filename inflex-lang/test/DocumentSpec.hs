{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

-- | Document loading spec.

module DocumentSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.UUID as UUID
import           Data.UUID.V4
import           Inflex.Document
import           Inflex.Instances ()
import           Inflex.Resolver
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import           Test.Hspec

spec :: Spec
spec = do
  describe "Errors" errors
  describe "Success" success

errors :: SpecWith ()
errors = do
  it
    "x = x"
    (do u1 <- nextRandom'
        shouldBe
          (let loaded =
                 loadDocument
                   [ Named
                       { uuid = Uuid u1
                       , name = "x"
                       , thing = "x"
                       , order = 0
                       , code = "x"
                       }
                   ]
            in evalDocument (evalEnvironment loaded) (defaultDocument loaded))
          (Toposorted
             { unToposorted =
                 [ Named
                     { uuid = (Uuid u1)
                     , name = "x"
                     , thing = Left (CycleError ["x"])
                     , order = 0
                     , code = "x"
                     }
                 ]
             }))
  it
    "x = y"
    (do u1 <- nextRandom'
        shouldBe
          (let loaded =
                 loadDocument
                   [ Named
                       { uuid = Uuid u1
                       , name = "x"
                       , thing = "y"
                       , code = "y"
                       , order = 0
                       }
                   ]
            in evalDocument (evalEnvironment loaded) (defaultDocument loaded))
          (Toposorted
             { unToposorted =
                 [ Named
                     { uuid = (Uuid u1)
                     , name = "x"
                     , thing =
                         Left
                           (LoadGenerateError
                              (FillErrors
                                 (MissingGlobal (M.fromList []) "y" :| [])))
                     , code = "y"
                     , order = 0
                     }
                 ]
             }))
  it
    "y = 1; x = y y"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        shouldBe
          (let loaded =
                 loadDocument
                   [ Named
                       { uuid = Uuid u1
                       , name = "x"
                       , thing = "y y"
                       , code = "y y"
                       , order = 0
                       }
                   , Named
                       { uuid = Uuid u2
                       , name = "y"
                       , thing = "1"
                       , code = "1"
                       , order = 1
                       }
                   ]
            in evalDocument (evalEnvironment loaded) (defaultDocument loaded))
          (Toposorted
             { unToposorted =
                 [ Named
                     { uuid = (Uuid u2)
                     , name = "y"
                     , thing =
                         Right
                           (LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location = ExpressionCursor
                                    , number = IntegerNumber 1
                                    , typ =
                                        ConstantType
                                          (TypeConstant
                                             { location = ExpressionCursor
                                             , name = IntegerTypeName
                                             })
                                    })))
                     , order = 1
                     , code = "1"
                     }
                 , Named
                     { uuid = (Uuid u1)
                     , name = "x"
                     , thing =
                         Left
                           (LoadResolveError
                              (ResolverErrors
                                 (NoInstanceAndMono
                                    (TypeVariable
                                       { location =
                                           ApplyArgCursor ExpressionCursor
                                       , prefix = PolyPrefix
                                       , index = 1
                                       , kind = TypeKind
                                       }) :|
                                  [])))
                     , order = 0
                     , code = "y y"
                     }
                 ]
             }))

success :: SpecWith ()
success = do
  it
    "x = y + 2; z = 2; y = z * 3.1"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        u3 <- nextRandom'
        shouldBe
          (let loaded =
                 loadDocument
                   [ Named
                       { uuid = Uuid u1
                       , name = "x"
                       , thing = "y + 2"
                       , code = "y + 2"
                       , order = 0
                       }
                   , Named
                       { uuid = Uuid u2
                       , name = "y"
                       , thing = "z * 3.1"
                       , code = "z * 3.1"
                       , order = 1
                       }
                   , Named
                       { uuid = Uuid u3
                       , name = "z"
                       , thing = "2"
                       , code = "2"
                       , order = 2
                       }
                   ]
            in evalDocument (evalEnvironment loaded) (defaultDocument loaded))
          (Toposorted
             { unToposorted =
                 [ Named
                     { uuid = Uuid u3
                     , name = "z"
                     , order = 2
                     , code = "2"
                     , thing =
                         Right
                           (LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location = ExpressionCursor
                                    , number = IntegerNumber 2
                                    , typ =
                                        ConstantType
                                          (TypeConstant
                                             { location = ExpressionCursor
                                             , name = IntegerTypeName
                                             })
                                    })))
                     }
                 , Named
                     { uuid = Uuid u2
                     , name = "y"
                     , order = 1
                     , code = "z * 3.1"
                     , thing =
                         Right
                           (LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location = SteppedCursor
                                    , number =
                                        DecimalNumber
                                          (Decimal {places = 1, integer = 62})
                                    , typ =
                                        ConstantType
                                          (TypeConstant
                                             { location = ExpressionCursor
                                             , name = IntegerTypeName
                                             })
                                    })))
                     }
                 , Named
                     { uuid = Uuid u1
                     , name = "x"
                     , order = 0
                     , code = "y + 2"
                     , thing =
                         Right
                           (LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location = SteppedCursor
                                    , number =
                                        DecimalNumber
                                          (Decimal {places = 1, integer = 82})
                                    , typ =
                                        ConstantType
                                          (TypeConstant
                                             { location = ExpressionCursor
                                             , name = IntegerTypeName
                                             })
                                    })))
                     }
                 ]
             }))
  it
    "double = \\x -> x * 2; a = double 1; b = double 2.2"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        u3 <- nextRandom'
        shouldBe
          (let loaded =
                 loadDocument
                   [ Named
                       { uuid = Uuid u1
                       , name = "double"
                       , thing = "\\x -> x * 2"
                       , code = "\\x -> x * 2"
                       , order = 0
                       }
                   , Named
                       { uuid = Uuid u2
                       , name = "a"
                       , thing = "double 1"
                       , code = "double 1"
                       , order = 1
                       }
                   , Named
                       { uuid = Uuid u3
                       , name = "b"
                       , thing = "double 2.2"
                       , code = "double 2.2"
                       , order = 2
                       }
                   ]
            in evalDocument (evalEnvironment loaded) (defaultDocument loaded))
          (Toposorted
             { unToposorted =
                 [ Named
                     { uuid = (Uuid u1)
                     , name = "double"
                     , code = "\\x -> x * 2"
                     , order = 0
                     , thing =
                         Right
                           (LambdaExpression
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
                                     InfixExpression
                                       (Infix
                                          { location =
                                              LambdaBodyCursor ExpressionCursor
                                          , global =
                                              ApplyExpression
                                                (Apply
                                                   { location =
                                                       ImplicitlyApplicationOn
                                                         (LambdaBodyCursor
                                                            (InfixOpCursor
                                                               ExpressionCursor))
                                                   , function =
                                                       GlobalExpression
                                                         (Global
                                                            { location =
                                                                LambdaBodyCursor
                                                                  (InfixOpCursor
                                                                     ExpressionCursor)
                                                            , name =
                                                                NumericBinOpGlobal
                                                                  MulitplyOp
                                                            , scheme =
                                                                ResolvedScheme
                                                                  (ApplyType
                                                                     (TypeApplication
                                                                        { function =
                                                                            ApplyType
                                                                              (TypeApplication
                                                                                 { function =
                                                                                     ConstantType
                                                                                       (TypeConstant
                                                                                          { location =
                                                                                              LambdaBodyCursor
                                                                                                (InfixOpCursor
                                                                                                   ExpressionCursor)
                                                                                          , name =
                                                                                              FunctionTypeName
                                                                                          })
                                                                                 , argument =
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
                                                                                 , location =
                                                                                     LambdaBodyCursor
                                                                                       (InfixOpCursor
                                                                                          ExpressionCursor)
                                                                                 , kind =
                                                                                     TypeKind
                                                                                 })
                                                                        , argument =
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
                                                                        , location =
                                                                            LambdaBodyCursor
                                                                              (InfixOpCursor
                                                                                 ExpressionCursor)
                                                                        , kind =
                                                                            TypeKind
                                                                        }))
                                                            })
                                                   , argument =
                                                       GlobalExpression
                                                         (Global
                                                            { location =
                                                                AutoInsertedForDefaulterCursor
                                                            , name =
                                                                InstanceGlobal
                                                                  (IntegerOpInstance
                                                                     MulitplyOp)
                                                            , scheme =
                                                                ResolvedScheme
                                                                  (ApplyType
                                                                     (TypeApplication
                                                                        { function =
                                                                            ApplyType
                                                                              (TypeApplication
                                                                                 { function =
                                                                                     ConstantType
                                                                                       (TypeConstant
                                                                                          { location =
                                                                                              BuiltIn
                                                                                          , name =
                                                                                              IntegerTypeName
                                                                                          })
                                                                                 , argument =
                                                                                     ConstantType
                                                                                       (TypeConstant
                                                                                          { location =
                                                                                              BuiltIn
                                                                                          , name =
                                                                                              IntegerTypeName
                                                                                          })
                                                                                 , location =
                                                                                     BuiltIn
                                                                                 , kind =
                                                                                     TypeKind
                                                                                 })
                                                                        , argument =
                                                                            ConstantType
                                                                              (TypeConstant
                                                                                 { location =
                                                                                     BuiltIn
                                                                                 , name =
                                                                                     IntegerTypeName
                                                                                 })
                                                                        , location =
                                                                            BuiltIn
                                                                        , kind =
                                                                            TypeKind
                                                                        }))
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
                                          , left =
                                              VariableExpression
                                                (Variable
                                                   { location =
                                                       LambdaBodyCursor
                                                         (InfixLeftCursor
                                                            ExpressionCursor)
                                                   , name =
                                                       DeBrujinIndex
                                                         (DeBrujinNesting 0)
                                                   , typ =
                                                       PolyType
                                                         (TypeVariable
                                                            { location = ()
                                                            , prefix = ()
                                                            , index = 0
                                                            , kind = TypeKind
                                                            })
                                                   })
                                          , right =
                                              ApplyExpression
                                                (Apply
                                                   { location =
                                                       AutogeneratedCursor
                                                   , function =
                                                       ApplyExpression
                                                         (Apply
                                                            { location =
                                                                ImplicitlyApplicationOn
                                                                  AutogeneratedCursor
                                                            , function =
                                                                GlobalExpression
                                                                  (Global
                                                                     { location =
                                                                         AutogeneratedCursor
                                                                     , name =
                                                                         FromIntegerGlobal
                                                                     , scheme =
                                                                         ResolvedScheme
                                                                           (ApplyType
                                                                              (TypeApplication
                                                                                 { function =
                                                                                     ApplyType
                                                                                       (TypeApplication
                                                                                          { function =
                                                                                              ConstantType
                                                                                                (TypeConstant
                                                                                                   { location =
                                                                                                       AutogeneratedCursor
                                                                                                   , name =
                                                                                                       FunctionTypeName
                                                                                                   })
                                                                                          , argument =
                                                                                              ConstantType
                                                                                                (TypeConstant
                                                                                                   { location =
                                                                                                       AutogeneratedCursor
                                                                                                   , name =
                                                                                                       IntegerTypeName
                                                                                                   })
                                                                                          , location =
                                                                                              AutogeneratedCursor
                                                                                          , kind =
                                                                                              FunKind
                                                                                                TypeKind
                                                                                                TypeKind
                                                                                          })
                                                                                 , argument =
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
                                                                                 , location =
                                                                                     AutogeneratedCursor
                                                                                 , kind =
                                                                                     TypeKind
                                                                                 }))
                                                                     })
                                                            , argument =
                                                                GlobalExpression
                                                                  (Global
                                                                     { location =
                                                                         AutoInsertedForDefaulterCursor
                                                                     , name =
                                                                         InstanceGlobal
                                                                           FromIntegerIntegerInstance
                                                                     , scheme =
                                                                         ResolvedScheme
                                                                           (ApplyType
                                                                              (TypeApplication
                                                                                 { function =
                                                                                     ConstantType
                                                                                       (TypeConstant
                                                                                          { location =
                                                                                              BuiltIn
                                                                                          , name =
                                                                                              IntegerTypeName
                                                                                          })
                                                                                 , argument =
                                                                                     ConstantType
                                                                                       (TypeConstant
                                                                                          { location =
                                                                                              BuiltIn
                                                                                          , name =
                                                                                              IntegerTypeName
                                                                                          })
                                                                                 , location =
                                                                                     BuiltIn
                                                                                 , kind =
                                                                                     TypeKind
                                                                                 }))
                                                                     })
                                                            , typ =
                                                                PolyType
                                                                  (TypeVariable
                                                                     { location =
                                                                         ()
                                                                     , prefix =
                                                                         ()
                                                                     , index = 0
                                                                     , kind =
                                                                         TypeKind
                                                                     })
                                                            })
                                                   , argument =
                                                       LiteralExpression
                                                         (NumberLiteral
                                                            (Number
                                                               { location =
                                                                   LambdaBodyCursor
                                                                     (InfixRightCursor
                                                                        ExpressionCursor)
                                                               , number =
                                                                   IntegerNumber
                                                                     2
                                                               , typ =
                                                                   ConstantType
                                                                     (TypeConstant
                                                                        { location =
                                                                            LambdaBodyCursor
                                                                              (InfixRightCursor
                                                                                 ExpressionCursor)
                                                                        , name =
                                                                            IntegerTypeName
                                                                        })
                                                               }))
                                                   , typ =
                                                       PolyType
                                                         (TypeVariable
                                                            { location = ()
                                                            , prefix = ()
                                                            , index = 0
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
                                                                ExpressionCursor
                                                            , name =
                                                                FunctionTypeName
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
                                          , location = ExpressionCursor
                                          , kind = TypeKind
                                          })
                                 }))
                     }
                 , Named
                     { uuid = (Uuid u3)
                     , name = "b"
                     , order = 2
                     , code = "double 2.2"
                     , thing =
                         Right
                           (LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location = SteppedCursor
                                    , number =
                                        DecimalNumber
                                          (Decimal {places = 1, integer = 44})
                                    , typ =
                                        ApplyType
                                          (TypeApplication
                                             { function =
                                                 ConstantType
                                                   (TypeConstant
                                                      { location =
                                                          ApplyArgCursor
                                                            ExpressionCursor
                                                      , name = DecimalTypeName
                                                      })
                                             , argument =
                                                 ConstantType
                                                   (TypeConstant
                                                      { location =
                                                          ApplyArgCursor
                                                            ExpressionCursor
                                                      , name = NatTypeName 1
                                                      })
                                             , location =
                                                 ApplyArgCursor ExpressionCursor
                                             , kind = TypeKind
                                             })
                                    })))
                     }
                 , Named
                     { uuid = (Uuid u2)
                     , name = "a"
                     , order = 1
                     , code = "double 1"
                     , thing =
                         Right
                           (LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location = SteppedCursor
                                    , number = IntegerNumber 2
                                    , typ =
                                        ConstantType
                                          (TypeConstant
                                             { location =
                                                 ApplyArgCursor ExpressionCursor
                                             , name = IntegerTypeName
                                             })
                                    })))
                     }
                 ]
             }))

nextRandom' :: IO Text
nextRandom' = fmap UUID.toText nextRandom
