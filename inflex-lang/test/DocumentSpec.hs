{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

-- | Document loading spec.

module DocumentSpec where

import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID as UUID
import           Data.UUID.V4
import qualified Data.Vector as V
import           Inflex.Defaulter
import           Inflex.Document
import           Inflex.Instances ()
import           Inflex.Resolver
import           Inflex.Solver
import           Inflex.Stepper
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import qualified RIO
import           Test.Hspec

defaultDocument' :: Toposorted (Named (Either LoadError (IsResolved (Expression Resolved)))) -> IO (Toposorted (Named (Either LoadError Cell)))
defaultDocument' = RIO.runRIO DefaulterReader . defaultDocument

loadDocument' :: [Named Text] -> IO (Toposorted (Named (Either LoadError (IsResolved (Expression Resolved)))))
loadDocument' = RIO.runRIO DocumentReader . loadDocument

evalDocument' ::
     RIO.MonadIO m
  => RIO.Map Hash (Expression Resolved)
  -> m (Toposorted (Named (Either LoadError Cell)))
  -> m [Named (Either LoadError (Expression Resolved))]
evalDocument' env m = do
  doc <- m
  fmap
    (sortBy (comparing order) . unToposorted)
    (RIO.runRIO StepReader (evalDocument env doc))

spec :: Spec
spec = do
  describe "Errors" errors
  describe "Success" success
  describe "Regression tests" regression

errors :: SpecWith ()
errors = do
  it
    "x = x"
    (do u1 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { uuid = Uuid u1
                      , name = "x"
                      , thing = "x"
                      , order = 0
                      , code = "x"
                      }
                  ]
              (evalDocument' (evalEnvironment loaded)) (defaultDocument' loaded))
          [ Named
              { uuid = (Uuid u1)
              , name = "x"
              , thing = Left (CycleError [Uuid u1])
              , order = 0
              , code = "x"
              }
          ])
  it
    "x = y"
    (do u1 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { uuid = Uuid u1
                      , name = "x"
                      , thing = "y"
                      , code = "y"
                      , order = 0
                      }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { uuid = (Uuid u1)
              , name = "x"
              , thing =
                  Left
                    (LoadGenerateError
                       (FillErrors (MissingGlobal emptyFillerEnv "y" :| [])))
              , code = "y"
              , order = 0
              }
          ])
  it
    "y = 1; x = y y"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { uuid = Uuid u1
                      , name = "x"
                      , thing = "y(y)"
                      , code = "y(y)"
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
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { uuid = Uuid u1
              , name = "x"
              , order = 0
              , code = "y(y)"
              , thing =
                  Left
                    (LoadResolveError
                       (ResolverErrors
                          (NoInstanceForType
                             FromIntegerClassName
                             (ApplyType
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
                                                         DefaultedCursor
                                                     , name = IntegerTypeName
                                                     })
                                            , location = ExpressionCursor
                                            , kind = FunKind TypeKind TypeKind
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
                                   , kind = TypeKind
                                   })) :|
                           [])))
              }
          , Named
              { uuid = Uuid u2
              , name = "y"
              , order = 1
              , code = "1"
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
              }
          ])
  eval_it
    "Missing field"
    [("x", "{a:3}.z")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:3}.z"
           , thing =
               Left
                 (LoadSolveError
                    (SolverError
                       (RowMismatch
                          (TypeRow
                             { location = ExpressionCursor
                             , typeVariable =
                                 Just
                                   (TypeVariable
                                      { location = ExpressionCursor
                                      , prefix = RowVarPrefix
                                      , index = 0
                                      , kind = RowKind
                                      })
                             , fields =
                                 [ Field
                                     { location = ExpressionCursor
                                     , name = FieldName {unFieldName = "z"}
                                     , typ =
                                         VariableType
                                           (TypeVariable
                                              { location = ExpressionCursor
                                              , prefix = FieldTypePrefix
                                              , index = 3
                                              , kind = TypeKind
                                              })
                                     }
                                 ]
                             })
                          (TypeRow
                             { location = PropExpressionCursor ExpressionCursor
                             , typeVariable = Nothing
                             , fields =
                                 [ Field
                                     { location =
                                         PropExpressionCursor
                                           (RecordFieldCursor
                                              (FieldName {unFieldName = "a"})
                                              TypeCursor)
                                     , name = FieldName {unFieldName = "a"}
                                     , typ =
                                         VariableType
                                           (TypeVariable
                                              { location =
                                                  PropExpressionCursor
                                                    (RecordFieldCursor
                                                       (FieldName
                                                          {unFieldName = "a"})
                                                       (RowFieldExpression
                                                          ExpressionCursor))
                                              , prefix = ApplyPrefix
                                              , index = 2
                                              , kind = TypeKind
                                              })
                                     }
                                 ]
                             }))))
           }
       ])

success :: SpecWith ()
success = do
  it
    "x = 1; y = @uuid:x"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { uuid = Uuid u1
                      , name = "x"
                      , thing = "1"
                      , code = "1"
                      , order = 0
                      }
                  , let code = "@uuid:" <> u1
                     in Named
                          { uuid = Uuid u2
                          , name = "y"
                          , thing = code
                          , code = code
                          , order = 1
                          }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { uuid = Uuid u1
              , name = "x"
              , order = 0
              , code = "1"
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
              }
          , Named
              { uuid = Uuid u2
              , name = "y"
              , order = 1
              , code = "@uuid:" <> u1
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
              }
          ])
  it
    "x = y + 2; z = 2; y = z * 3.1"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        u3 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
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
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
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
          ])
  it
    "double = x: x * 2; a = double(1); b = double(2.2)"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        u3 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { uuid = Uuid u1
                      , name = "double"
                      , thing = "x: x * 2"
                      , code = "x: x * 2"
                      , order = 0
                      }
                  , Named
                      { uuid = Uuid u2
                      , name = "a"
                      , thing = "double(1)"
                      , code = "double(1)"
                      , order = 1
                      }
                  , Named
                      { uuid = Uuid u3
                      , name = "b"
                      , thing = "double(2.2)"
                      , code = "double(2.2)"
                      , order = 2
                      }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { uuid = Uuid u1
              , name = "double"
              , order = 0
              , code = "x: x * 2"
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
                                                                                       BuiltIn
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
                                                                              BuiltIn
                                                                          , kind =
                                                                              FunKind
                                                                                TypeKind
                                                                                TypeKind
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
                                                                                                BuiltIn
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
                                                                                       BuiltIn
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
                                                                              BuiltIn
                                                                          , kind =
                                                                              TypeKind
                                                                          })
                                                                 , location =
                                                                     BuiltIn
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
                                                                                       FunctionTypeName
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
                                                                              FunKind
                                                                                TypeKind
                                                                                TypeKind
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
                                                                                                BuiltIn
                                                                                            , name =
                                                                                                FunctionTypeName
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
                                                                                       FunKind
                                                                                         TypeKind
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
                                            { location = BuiltIn
                                            , function =
                                                ApplyExpression
                                                  (Apply
                                                     { location =
                                                         ImplicitlyApplicationOn
                                                           BuiltIn
                                                     , function =
                                                         GlobalExpression
                                                           (Global
                                                              { location =
                                                                  BuiltIn
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
                                                                                                BuiltIn
                                                                                            , name =
                                                                                                FunctionTypeName
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
                                                                              BuiltIn
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
                                                                              ApplyType
                                                                                (TypeApplication
                                                                                   { function =
                                                                                       ConstantType
                                                                                         (TypeConstant
                                                                                            { location =
                                                                                                BuiltIn
                                                                                            , name =
                                                                                                FunctionTypeName
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
                                                                                       FunKind
                                                                                         TypeKind
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
                                            , argument =
                                                LiteralExpression
                                                  (NumberLiteral
                                                     (Number
                                                        { location =
                                                            LambdaBodyCursor
                                                              (InfixRightCursor
                                                                 ExpressionCursor)
                                                        , number =
                                                            IntegerNumber 2
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
                                            , index = 0
                                            , kind = TypeKind
                                            })
                                   , location = ExpressionCursor
                                   , kind = TypeKind
                                   })
                          }))
              }
          , Named
              { uuid = Uuid u2
              , name = "a"
              , order = 1
              , code = "double(1)"
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
          , Named
              { uuid = Uuid u3
              , name = "b"
              , order = 2
              , code = "double(2.2)"
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
          ])
  describe "Records" records

records :: Spec
records = do
  eval_it
    "Arith inside a record is fine"
    [("x", "{a:3*2}")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:3*2}"
           , thing =
               Right
                 (RecordExpression
                    (Record
                       { fields =
                           [ FieldE
                               { name = FieldName {unFieldName = "a"}
                               , expression =
                                   LiteralExpression
                                     (NumberLiteral
                                        (Number
                                           { location = SteppedCursor
                                           , number = IntegerNumber 6
                                           , typ =
                                               ConstantType
                                                 (TypeConstant
                                                    { location =
                                                        RecordFieldCursor
                                                          (FieldName
                                                             {unFieldName = "a"})
                                                          (RowFieldExpression
                                                             (InfixLeftCursor
                                                                ExpressionCursor))
                                                    , name = IntegerTypeName
                                                    })
                                           }))
                               , location =
                                   RecordFieldCursor
                                     (FieldName {unFieldName = "a"})
                                     TypeCursor
                               }
                           ]
                       , location = ExpressionCursor
                       , typ =
                           RecordType
                             (RowType
                                (TypeRow
                                   { location = ExpressionCursor
                                   , typeVariable = Nothing
                                   , fields =
                                       [ Field
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "a"})
                                                 TypeCursor
                                           , name =
                                               FieldName {unFieldName = "a"}
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
                                   }))
                       }))
           }
       ])
  eval_it
    "Arith referencing a unary record is fine"
    [("x", "{a:666}"), ("y", "x.a * 2")]
    (\[u1, u2] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:666}"
           , thing =
               Right
                 (RecordExpression
                    (Record
                       { fields =
                           [ FieldE
                               { name = FieldName {unFieldName = "a"}
                               , expression =
                                   LiteralExpression
                                     (NumberLiteral
                                        (Number
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "a"})
                                                 (RowFieldExpression
                                                    ExpressionCursor)
                                           , number = IntegerNumber 666
                                           , typ =
                                               ConstantType
                                                 (TypeConstant
                                                    { location =
                                                        RecordFieldCursor
                                                          (FieldName
                                                             {unFieldName = "a"})
                                                          (RowFieldExpression
                                                             ExpressionCursor)
                                                    , name = IntegerTypeName
                                                    })
                                           }))
                               , location =
                                   RecordFieldCursor
                                     (FieldName {unFieldName = "a"})
                                     TypeCursor
                               }
                           ]
                       , location = ExpressionCursor
                       , typ =
                           RecordType
                             (RowType
                                (TypeRow
                                   { location = ExpressionCursor
                                   , typeVariable = Nothing
                                   , fields =
                                       [ Field
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "a"})
                                                 TypeCursor
                                           , name =
                                               FieldName {unFieldName = "a"}
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
                                   }))
                       }))
           }
       , Named
           { uuid = Uuid u2
           , name = "y"
           , order = 1
           , code = "x.a * 2"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location = SteppedCursor
                          , number = IntegerNumber 1332
                          , typ =
                              ConstantType
                                (TypeConstant
                                   { location =
                                       RecordFieldCursor
                                         (FieldName {unFieldName = "a"})
                                         (RowFieldExpression ExpressionCursor)
                                   , name = IntegerTypeName
                                   })
                          })))
           }
       ])
  eval_it
    "Referencing a single field from a 2-ary record is fine (without type sig)"
    [("x", "{a:1, b:8}"), ("y", "x.a")]
    (\[u1, u2] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:1, b:8}"
           , thing =
               Right
                 (RecordExpression
                    (Record
                       { fields =
                           [ FieldE
                               { name = FieldName {unFieldName = "a"}
                               , expression =
                                   LiteralExpression
                                     (NumberLiteral
                                        (Number
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "a"})
                                                 (RowFieldExpression
                                                    ExpressionCursor)
                                           , number = IntegerNumber 1
                                           , typ =
                                               ConstantType
                                                 (TypeConstant
                                                    { location =
                                                        RecordFieldCursor
                                                          (FieldName
                                                             {unFieldName = "a"})
                                                          (RowFieldExpression
                                                             ExpressionCursor)
                                                    , name = IntegerTypeName
                                                    })
                                           }))
                               , location =
                                   RecordFieldCursor
                                     (FieldName {unFieldName = "a"})
                                     TypeCursor
                               }
                           , FieldE
                               { name = FieldName {unFieldName = "b"}
                               , expression =
                                   LiteralExpression
                                     (NumberLiteral
                                        (Number
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "b"})
                                                 (RowFieldExpression
                                                    ExpressionCursor)
                                           , number = IntegerNumber 8
                                           , typ =
                                               ConstantType
                                                 (TypeConstant
                                                    { location =
                                                        RecordFieldCursor
                                                          (FieldName
                                                             {unFieldName = "b"})
                                                          (RowFieldExpression
                                                             ExpressionCursor)
                                                    , name = IntegerTypeName
                                                    })
                                           }))
                               , location =
                                   RecordFieldCursor
                                     (FieldName {unFieldName = "b"})
                                     TypeCursor
                               }
                           ]
                       , location = ExpressionCursor
                       , typ =
                           RecordType
                             (RowType
                                (TypeRow
                                   { location = ExpressionCursor
                                   , typeVariable = Nothing
                                   , fields =
                                       [ Field
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "a"})
                                                 TypeCursor
                                           , name =
                                               FieldName {unFieldName = "a"}
                                           , typ =
                                               PolyType
                                                 (TypeVariable
                                                    { location = ()
                                                    , prefix = ()
                                                    , index = 0
                                                    , kind = TypeKind
                                                    })
                                           }
                                       , Field
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "b"})
                                                 TypeCursor
                                           , name =
                                               FieldName {unFieldName = "b"}
                                           , typ =
                                               PolyType
                                                 (TypeVariable
                                                    { location = ()
                                                    , prefix = ()
                                                    , index = 1
                                                    , kind = TypeKind
                                                    })
                                           }
                                       ]
                                   }))
                       }))
           }
       , Named
           { uuid = Uuid u2
           , name = "y"
           , order = 1
           , code = "x.a"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location =
                              RecordFieldCursor
                                (FieldName {unFieldName = "a"})
                                (RowFieldExpression ExpressionCursor)
                          , number = IntegerNumber 1
                          , typ =
                              ConstantType
                                (TypeConstant
                                   { location =
                                       RecordFieldCursor
                                         (FieldName {unFieldName = "a"})
                                         (RowFieldExpression ExpressionCursor)
                                   , name = IntegerTypeName
                                   })
                          })))
           }
       ])
  -- This example demonstrates that if the `b' field has a type
  -- annotation, then there is no class inference issue.
  eval_it
    "Referencing a single field from a 2-ary record is fine (with type sig)"
    [("x", "{a:1, b:8 :: Integer}"), ("y", "x.a")]
    (\[u1, u2] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:1, b:8 :: Integer}"
           , thing =
               Right
                 (RecordExpression
                    (Record
                       { fields =
                           [ FieldE
                               { name = FieldName {unFieldName = "a"}
                               , expression =
                                   LiteralExpression
                                     (NumberLiteral
                                        (Number
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "a"})
                                                 (RowFieldExpression
                                                    ExpressionCursor)
                                           , number = IntegerNumber 1
                                           , typ =
                                               ConstantType
                                                 (TypeConstant
                                                    { location =
                                                        RecordFieldCursor
                                                          (FieldName
                                                             {unFieldName = "a"})
                                                          (RowFieldExpression
                                                             ExpressionCursor)
                                                    , name = IntegerTypeName
                                                    })
                                           }))
                               , location =
                                   RecordFieldCursor
                                     (FieldName {unFieldName = "a"})
                                     TypeCursor
                               }
                           , FieldE
                               { name = FieldName {unFieldName = "b"}
                               , expression =
                                   LiteralExpression
                                     (NumberLiteral
                                        (Number
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "b"})
                                                 (RowFieldExpression
                                                    ExpressionCursor)
                                           , number = IntegerNumber 8
                                           , typ =
                                               ConstantType
                                                 (TypeConstant
                                                    { location =
                                                        RecordFieldCursor
                                                          (FieldName
                                                             {unFieldName = "b"})
                                                          (RowFieldExpression
                                                             ExpressionCursor)
                                                    , name = IntegerTypeName
                                                    })
                                           }))
                               , location =
                                   RecordFieldCursor
                                     (FieldName {unFieldName = "b"})
                                     TypeCursor
                               }
                           ]
                       , location = ExpressionCursor
                       , typ =
                           RecordType
                             (RowType
                                (TypeRow
                                   { location = ExpressionCursor
                                   , typeVariable = Nothing
                                   , fields =
                                       [ Field
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "a"})
                                                 TypeCursor
                                           , name =
                                               FieldName {unFieldName = "a"}
                                           , typ =
                                               PolyType
                                                 (TypeVariable
                                                    { location = ()
                                                    , prefix = ()
                                                    , index = 0
                                                    , kind = TypeKind
                                                    })
                                           }
                                       , Field
                                           { location =
                                               RecordFieldCursor
                                                 (FieldName {unFieldName = "b"})
                                                 TypeCursor
                                           , name =
                                               FieldName {unFieldName = "b"}
                                           , typ =
                                               ConstantType
                                                 (TypeConstant
                                                    { location =
                                                        RecordFieldCursor
                                                          (FieldName
                                                             {unFieldName = "b"})
                                                          (RowFieldExpression
                                                             ExpressionCursor)
                                                    , name = IntegerTypeName
                                                    })
                                           }
                                       ]
                                   }))
                       }))
           }
       , Named
           { uuid = Uuid u2
           , name = "y"
           , order = 1
           , code = "x.a"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location =
                              RecordFieldCursor
                                (FieldName {unFieldName = "a"})
                                (RowFieldExpression ExpressionCursor)
                          , number = IntegerNumber 1
                          , typ =
                              ConstantType
                                (TypeConstant
                                   { location =
                                       RecordFieldCursor
                                         (FieldName {unFieldName = "a"})
                                         (RowFieldExpression ExpressionCursor)
                                   , name = IntegerTypeName
                                   })
                          })))
           }
       ])
  eval_it
    "Defaulting for a verbatim copy of a record is also fine:"
    [("x", "{a:1, b:8}"), ("y", "x")]
    (\[u1, u2] ->
       let record =
             RecordExpression
               (Record
                  { fields =
                      [ FieldE
                          { name = FieldName {unFieldName = "a"}
                          , expression =
                              LiteralExpression
                                (NumberLiteral
                                   (Number
                                      { location =
                                          RecordFieldCursor
                                            (FieldName {unFieldName = "a"})
                                            (RowFieldExpression ExpressionCursor)
                                      , number = IntegerNumber 1
                                      , typ =
                                          ConstantType
                                            (TypeConstant
                                               { location =
                                                   RecordFieldCursor
                                                     (FieldName
                                                        {unFieldName = "a"})
                                                     (RowFieldExpression
                                                        ExpressionCursor)
                                               , name = IntegerTypeName
                                               })
                                      }))
                          , location =
                              RecordFieldCursor
                                (FieldName {unFieldName = "a"})
                                TypeCursor
                          }
                      , FieldE
                          { name = FieldName {unFieldName = "b"}
                          , expression =
                              LiteralExpression
                                (NumberLiteral
                                   (Number
                                      { location =
                                          RecordFieldCursor
                                            (FieldName {unFieldName = "b"})
                                            (RowFieldExpression ExpressionCursor)
                                      , number = IntegerNumber 8
                                      , typ =
                                          ConstantType
                                            (TypeConstant
                                               { location =
                                                   RecordFieldCursor
                                                     (FieldName
                                                        {unFieldName = "b"})
                                                     (RowFieldExpression
                                                        ExpressionCursor)
                                               , name = IntegerTypeName
                                               })
                                      }))
                          , location =
                              RecordFieldCursor
                                (FieldName {unFieldName = "b"})
                                TypeCursor
                          }
                      ]
                  , location = ExpressionCursor
                  , typ =
                      RecordType
                        (RowType
                           (TypeRow
                              { location = ExpressionCursor
                              , typeVariable = Nothing
                              , fields =
                                  [ Field
                                      { location =
                                          RecordFieldCursor
                                            (FieldName {unFieldName = "a"})
                                            TypeCursor
                                      , name = FieldName {unFieldName = "a"}
                                      , typ =
                                          PolyType
                                            (TypeVariable
                                               { location = ()
                                               , prefix = ()
                                               , index = 0
                                               , kind = TypeKind
                                               })
                                      }
                                  , Field
                                      { location =
                                          RecordFieldCursor
                                            (FieldName {unFieldName = "b"})
                                            TypeCursor
                                      , name = FieldName {unFieldName = "b"}
                                      , typ =
                                          PolyType
                                            (TypeVariable
                                               { location = ()
                                               , prefix = ()
                                               , index = 1
                                               , kind = TypeKind
                                               })
                                      }
                                  ]
                              }))
                  })
        in [ Named
               { uuid = Uuid u1
               , name = "x"
               , order = 0
               , code = "{a:1, b:8}"
               , thing = Right record
               }
           , Named
               { uuid = Uuid u2
               , name = "y"
               , order = 1
               , code = "x"
               , thing = Right record
               }
           ])
  eval_it
    "Immediate access of a record's field is fine"
    [("x", "{a:1}.a")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:1}.a"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location =
                              PropExpressionCursor
                                (RecordFieldCursor
                                   (FieldName {unFieldName = "a"})
                                   (RowFieldExpression ExpressionCursor))
                          , number = IntegerNumber 1
                          , typ =
                              ConstantType
                                (TypeConstant
                                   { location =
                                       PropExpressionCursor
                                         (RecordFieldCursor
                                            (FieldName {unFieldName = "a"})
                                            (RowFieldExpression ExpressionCursor))
                                   , name = IntegerTypeName
                                   })
                          })))
           }
       ])
  eval_it
    "Nested record fields"
    [("x", "({a:{b:2}}.a).b")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "({a:{b:2}}.a).b"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location =
                              PropExpressionCursor
                                (PropExpressionCursor
                                   (RecordFieldCursor
                                      (FieldName {unFieldName = "a"})
                                      (RowFieldExpression
                                         (RecordFieldCursor
                                            (FieldName {unFieldName = "b"})
                                            (RowFieldExpression ExpressionCursor)))))
                          , number = IntegerNumber 2
                          , typ =
                              ConstantType
                                (TypeConstant
                                   { location =
                                       PropExpressionCursor
                                         (PropExpressionCursor
                                            (RecordFieldCursor
                                               (FieldName {unFieldName = "a"})
                                               (RowFieldExpression
                                                  (RecordFieldCursor
                                                     (FieldName
                                                        {unFieldName = "b"})
                                                     (RowFieldExpression
                                                        ExpressionCursor)))))
                                   , name = IntegerTypeName
                                   })
                          })))
           }
       ])
  eval_it
    "Multiply record fields"
    [("x", "{a:3,b:2}.a * {x:1,y:2}.y")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:3,b:2}.a * {x:1,y:2}.y"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location = SteppedCursor
                          , number = IntegerNumber 6
                          , typ =
                              ConstantType
                                (TypeConstant
                                   { location =
                                       InfixLeftCursor
                                         (PropExpressionCursor
                                            (RecordFieldCursor
                                               (FieldName {unFieldName = "a"})
                                               (RowFieldExpression
                                                  ExpressionCursor)))
                                   , name = IntegerTypeName
                                   })
                          })))
           }
       ])
  eval_it
    "Multiply record fields (decimal version; one side decimal)"
    [("x", "{a:3.1,b:2}.a * {x:1,y:2}.y")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:3.1,b:2}.a * {x:1,y:2}.y"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location = SteppedCursor
                          , number =
                              DecimalNumber (Decimal {places = 1, integer = 62})
                          , typ =
                              ApplyType
                                (TypeApplication
                                   { function =
                                       ConstantType
                                         (TypeConstant
                                            { location =
                                                InfixLeftCursor
                                                  (PropExpressionCursor
                                                     (RecordFieldCursor
                                                        (FieldName
                                                           {unFieldName = "a"})
                                                        (RowFieldExpression
                                                           ExpressionCursor)))
                                            , name = DecimalTypeName
                                            })
                                   , argument =
                                       ConstantType
                                         (TypeConstant
                                            { location =
                                                InfixLeftCursor
                                                  (PropExpressionCursor
                                                     (RecordFieldCursor
                                                        (FieldName
                                                           {unFieldName = "a"})
                                                        (RowFieldExpression
                                                           ExpressionCursor)))
                                            , name = NatTypeName 1
                                            })
                                   , location =
                                       InfixLeftCursor
                                         (PropExpressionCursor
                                            (RecordFieldCursor
                                               (FieldName {unFieldName = "a"})
                                               (RowFieldExpression
                                                  ExpressionCursor)))
                                   , kind = TypeKind
                                   })
                          })))
           }
       ])
  eval_it
    "Multiply record fields (decimal version; both sides decimal)"
    [("x", "{a:3.1,b:2}.a * {x:1,y:2.51}.y")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "{a:3.1,b:2}.a * {x:1,y:2.51}.y"
           , thing =
               Right
                 (LiteralExpression
                    (NumberLiteral
                       (Number
                          { location = SteppedCursor
                          , number =
                              DecimalNumber
                                (Decimal {places = 2, integer = 778})
                          , typ =
                              ApplyType
                                (TypeApplication
                                   { function =
                                       ConstantType
                                         (TypeConstant
                                            { location =
                                                InfixLeftCursor
                                                  (PropExpressionCursor
                                                     (RecordFieldCursor
                                                        (FieldName
                                                           {unFieldName = "a"})
                                                        (RowFieldExpression
                                                           ExpressionCursor)))
                                            , name = DecimalTypeName
                                            })
                                   , argument =
                                       ConstantType
                                         (TypeConstant
                                            { location =
                                                InfixLeftCursor
                                                  (PropExpressionCursor
                                                     (RecordFieldCursor
                                                        (FieldName
                                                           {unFieldName = "a"})
                                                        (RowFieldExpression
                                                           ExpressionCursor)))
                                            , name = NatTypeName 1
                                            })
                                   , location =
                                       InfixLeftCursor
                                         (PropExpressionCursor
                                            (RecordFieldCursor
                                               (FieldName {unFieldName = "a"})
                                               (RowFieldExpression
                                                  ExpressionCursor)))
                                   , kind = TypeKind
                                   })
                          })))
           }
       ])
  eval_it
    "Arrays"
    [("x", "[1.0,2*3]")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "[1.0,2*3]"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { expressions =
                           V.fromList
                             [ LiteralExpression
                                 (NumberLiteral
                                    (Number
                                       { location = BuiltIn
                                       , number =
                                           DecimalNumber
                                             (Decimal {places = 1, integer = 10})
                                       , typ =
                                           ApplyType
                                             (TypeApplication
                                                { function =
                                                    ConstantType
                                                      (TypeConstant
                                                         { location =
                                                             ArrayElementCursor
                                                               0
                                                               ExpressionCursor
                                                         , name =
                                                             DecimalTypeName
                                                         })
                                                , argument =
                                                    ConstantType
                                                      (TypeConstant
                                                         { location =
                                                             ArrayElementCursor
                                                               0
                                                               ExpressionCursor
                                                         , name = NatTypeName 1
                                                         })
                                                , location =
                                                    ArrayElementCursor
                                                      0
                                                      ExpressionCursor
                                                , kind = TypeKind
                                                })
                                       }))
                             , LiteralExpression
                                 (NumberLiteral
                                    (Number
                                       { location = SteppedCursor
                                       , number =
                                           DecimalNumber
                                             (Decimal {places = 1, integer = 60})
                                       , typ =
                                           ConstantType
                                             (TypeConstant
                                                { location =
                                                    ArrayElementCursor
                                                      1
                                                      (InfixLeftCursor
                                                         ExpressionCursor)
                                                , name = IntegerTypeName
                                                })
                                       }))
                             ]
                       , typ =
                           ArrayType
                             (PolyType
                                (TypeVariable
                                   { location = ()
                                   , prefix = ()
                                   , index = 0
                                   , kind = TypeKind
                                   }))
                       , location = ExpressionCursor
                       }))
           }
       ])

regression :: Spec
regression = do
  error_while_evaluating_with_annotation
  duplicate_empty_names_ok
  mapfunc
  table_map_defaulting

mapfunc :: SpecWith ()
mapfunc =
  eval_it
    "map(x:x*2,[3])"
    [("x", "(map(x:x*2))([3])")]
    (\[u1] ->
       [ Named
           { uuid = Uuid u1
           , name = "x"
           , order = 0
           , code = "(map(x:x*2))([3])"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { expressions =
                           V.fromList
                             [ LiteralExpression
                                 (NumberLiteral
                                    (Number
                                       { location = SteppedCursor
                                       , number = IntegerNumber 6
                                       , typ =
                                           ConstantType
                                             (TypeConstant
                                                { location =
                                                    ApplyArgCursor
                                                      (ArrayElementCursor
                                                         0
                                                         ExpressionCursor)
                                                , name = IntegerTypeName
                                                })
                                       }))
                             ]
                       , typ =
                           ArrayType
                             (PolyType
                                (TypeVariable
                                   { location = ()
                                   , prefix = ()
                                   , index = 0
                                   , kind = TypeKind
                                   }))
                       , location = ApplyFuncCursor ExpressionCursor
                       }))
           }
       ])

table_map_defaulting :: SpecWith ()
table_map_defaulting =
  eval_it
    "t = [{x:3,y:2.0}]; k = map(r:r.x*r.y,t)"
    [("t", "[{x:3,y:2.0}]"), ("k", "map(r:r.x*r.y,t)")]
    (\[u1, u2] ->
       [ Named
           { uuid = Uuid u1
           , name = "t"
           , order = 0
           , code = "[{x:3,y:2.0}]"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { expressions =
                           V.fromList
                             [ RecordExpression
                                 (Record
                                    { fields =
                                        [ FieldE
                                            { name =
                                                FieldName {unFieldName = "x"}
                                            , expression =
                                                LiteralExpression
                                                  (NumberLiteral
                                                     (Number
                                                        { location =
                                                            ArrayElementCursor
                                                              0
                                                              (RecordFieldCursor
                                                                 (FieldName
                                                                    { unFieldName =
                                                                        "x"
                                                                    })
                                                                 (RowFieldExpression
                                                                    ExpressionCursor))
                                                        , number =
                                                            IntegerNumber 3
                                                        , typ =
                                                            ConstantType
                                                              (TypeConstant
                                                                 { location =
                                                                     ArrayElementCursor
                                                                       0
                                                                       (RecordFieldCursor
                                                                          (FieldName
                                                                             { unFieldName =
                                                                                 "x"
                                                                             })
                                                                          (RowFieldExpression
                                                                             ExpressionCursor))
                                                                 , name =
                                                                     IntegerTypeName
                                                                 })
                                                        }))
                                            , location =
                                                ArrayElementCursor
                                                  0
                                                  (RecordFieldCursor
                                                     (FieldName
                                                        {unFieldName = "x"})
                                                     TypeCursor)
                                            }
                                        , FieldE
                                            { name =
                                                FieldName {unFieldName = "y"}
                                            , expression =
                                                LiteralExpression
                                                  (NumberLiteral
                                                     (Number
                                                        { location =
                                                            BuiltIn
                                                        , number =
                                                            DecimalNumber
                                                              (Decimal
                                                                 { places = 1
                                                                 , integer = 20
                                                                 })
                                                        , typ =
                                                            ApplyType
                                                              (TypeApplication
                                                                 { function =
                                                                     ConstantType
                                                                       (TypeConstant
                                                                          { location =
                                                                              ArrayElementCursor
                                                                                0
                                                                                (RecordFieldCursor
                                                                                   (FieldName
                                                                                      { unFieldName =
                                                                                          "y"
                                                                                      })
                                                                                   (RowFieldExpression
                                                                                      ExpressionCursor))
                                                                          , name =
                                                                              DecimalTypeName
                                                                          })
                                                                 , argument =
                                                                     ConstantType
                                                                       (TypeConstant
                                                                          { location =
                                                                              ArrayElementCursor
                                                                                0
                                                                                (RecordFieldCursor
                                                                                   (FieldName
                                                                                      { unFieldName =
                                                                                          "y"
                                                                                      })
                                                                                   (RowFieldExpression
                                                                                      ExpressionCursor))
                                                                          , name =
                                                                              NatTypeName
                                                                                1
                                                                          })
                                                                 , location =
                                                                     ArrayElementCursor
                                                                       0
                                                                       (RecordFieldCursor
                                                                          (FieldName
                                                                             { unFieldName =
                                                                                 "y"
                                                                             })
                                                                          (RowFieldExpression
                                                                             ExpressionCursor))
                                                                 , kind =
                                                                     TypeKind
                                                                 })
                                                        }))
                                            , location =
                                                ArrayElementCursor
                                                  0
                                                  (RecordFieldCursor
                                                     (FieldName
                                                        {unFieldName = "y"})
                                                     TypeCursor)
                                            }
                                        ]
                                    , location =
                                        ArrayElementCursor 0 ExpressionCursor
                                    , typ =
                                        RecordType
                                          (RowType
                                             (TypeRow
                                                { location =
                                                    ArrayElementCursor
                                                      0
                                                      ExpressionCursor
                                                , typeVariable = Nothing
                                                , fields =
                                                    [ Field
                                                        { location =
                                                            ArrayElementCursor
                                                              0
                                                              (RecordFieldCursor
                                                                 (FieldName
                                                                    { unFieldName =
                                                                        "x"
                                                                    })
                                                                 TypeCursor)
                                                        , name =
                                                            FieldName
                                                              { unFieldName =
                                                                  "x"
                                                              }
                                                        , typ =
                                                            PolyType
                                                              (TypeVariable
                                                                 { location = ()
                                                                 , prefix = ()
                                                                 , index = 0
                                                                 , kind =
                                                                     TypeKind
                                                                 })
                                                        }
                                                    , Field
                                                        { location =
                                                            ArrayElementCursor
                                                              0
                                                              (RecordFieldCursor
                                                                 (FieldName
                                                                    { unFieldName =
                                                                        "y"
                                                                    })
                                                                 TypeCursor)
                                                        , name =
                                                            FieldName
                                                              { unFieldName =
                                                                  "y"
                                                              }
                                                        , typ =
                                                            PolyType
                                                              (TypeVariable
                                                                 { location = ()
                                                                 , prefix = ()
                                                                 , index = 1
                                                                 , kind =
                                                                     TypeKind
                                                                 })
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
                                      { location =
                                          ArrayElementCursor 0 ExpressionCursor
                                      , typeVariable = Nothing
                                      , fields =
                                          [ Field
                                              { location =
                                                  ArrayElementCursor
                                                    0
                                                    (RecordFieldCursor
                                                       (FieldName
                                                          {unFieldName = "x"})
                                                       TypeCursor)
                                              , name =
                                                  FieldName {unFieldName = "x"}
                                              , typ =
                                                  PolyType
                                                    (TypeVariable
                                                       { location = ()
                                                       , prefix = ()
                                                       , index = 0
                                                       , kind = TypeKind
                                                       })
                                              }
                                          , Field
                                              { location =
                                                  ArrayElementCursor
                                                    0
                                                    (RecordFieldCursor
                                                       (FieldName
                                                          {unFieldName = "y"})
                                                       TypeCursor)
                                              , name =
                                                  FieldName {unFieldName = "y"}
                                              , typ =
                                                  PolyType
                                                    (TypeVariable
                                                       { location = ()
                                                       , prefix = ()
                                                       , index = 1
                                                       , kind = TypeKind
                                                       })
                                              }
                                          ]
                                      })))
                       , location = ExpressionCursor
                       }))
           }
       , Named
           { uuid = Uuid u2
           , name = "k"
           , order = 1
           , code = "map(r:r.x*r.y,t)"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { expressions =
                           V.fromList
                             [ LiteralExpression
                                 (NumberLiteral
                                    (Number
                                       { location = SteppedCursor
                                       , number =
                                           DecimalNumber
                                             (Decimal {places = 1, integer = 60})
                                       , typ =
                                           ConstantType
                                             (TypeConstant
                                                { location =
                                                    ArrayElementCursor
                                                      0
                                                      (RecordFieldCursor
                                                         (FieldName
                                                            {unFieldName = "x"})
                                                         (RowFieldExpression
                                                            ExpressionCursor))
                                                , name = IntegerTypeName
                                                })
                                       }))
                             ]
                       , typ =
                           ArrayType
                             (PolyType
                                (TypeVariable
                                   { location = ()
                                   , prefix = ()
                                   , index = 0
                                   , kind = TypeKind
                                   }))
                       , location = ApplyFuncCursor ExpressionCursor
                       }))
           }
       ])

error_while_evaluating_with_annotation :: SpecWith ()
error_while_evaluating_with_annotation =
  it
    "Error while evaluating with annotation"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        let _loaded =
              loadDocument'
                [ Named
                    { uuid = Uuid u1
                    , name = "x"
                    , thing = "193 :: Decimal 2"
                    , code = "193 :: Decimal 2"
                    , order = 0
                    }
                , Named
                    { uuid = Uuid u2
                    , name = "z"
                    , thing = "x+2"
                    , code = "x+2"
                    , order = 1
                    }
                ]
        pendingWith "Need to fix the evaluator") -- TODO: Fix this!

duplicate_empty_names_ok :: Spec
duplicate_empty_names_ok =
  it
    "Duplicate empty string names should be ok"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        loaded <-
          loadDocument'
            [ Named
                { uuid = Uuid u1
                , name = ""
                , thing = "193"
                , code = "193"
                , order = 0
                }
            , Named
                { uuid = Uuid u2
                , name = ""
                , thing = "2+3"
                , code = "2+3"
                , order = 1
                }
            ]
        shouldReturn
          (evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { uuid = Uuid u1
              , name = ""
              , order = 0
              , code = "193"
              , thing =
                  Right
                    (LiteralExpression
                       (NumberLiteral
                          (Number
                             { location = ExpressionCursor
                             , number = IntegerNumber 193
                             , typ =
                                 ConstantType
                                   (TypeConstant
                                      { location = ExpressionCursor
                                      , name = IntegerTypeName
                                      })
                             })))
              }
          ,  Named
               { uuid = Uuid u2
               , name = ""
               , order = 1
               , code = "2+3"
               , thing =
                   Right
                     (LiteralExpression
                        (NumberLiteral
                           (Number
                              { location = SteppedCursor
                              , number = IntegerNumber 5
                              , typ =
                                  ConstantType
                                    (TypeConstant
                                       { location =
                                           InfixLeftCursor
                                             ExpressionCursor
                                       , name = IntegerTypeName
                                       })
                              })))
               }
          ])

--------------------------------------------------------------------------------
-- Helpers

eval_it ::
     String -> [(Text, Text)]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))])
  -> SpecWith ()
eval_it desc xs result = eval_it_with desc xs result (pure ())

eval_it_pending ::
     String -> [(Text, Text)]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))])
  -> String
  -> SpecWith ()
eval_it_pending desc xs result t = eval_it_with desc xs result (pendingWith t)

eval_it_with ::
     String -> [(Text, Text)]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))])
  -> IO ()
  -> SpecWith ()
eval_it_with desc xs result io =
  it
    (desc <> ": " <>
     intercalate
       "; "
       (map (\(name, val) -> T.unpack name <> " = " <> T.unpack val) xs))
    (do io
        xs' <-
          mapM
            (\x -> do
               u <- nextRandom'
               pure (u, x))
            xs
        shouldReturn
          (do loaded <-
                loadDocument'
                  (zipWith
                     (\i (uuid, (name, thing)) ->
                        Named
                          { uuid = Uuid uuid
                          , name
                          , thing
                          , code = thing
                          , order = i
                          })
                     [0 ..]
                     xs')
              evalDocument'
                (evalEnvironment loaded)
                (RIO.runRIO DefaulterReader (defaultDocument loaded)))
          (result (map fst xs')))

nextRandom' :: IO Text
nextRandom' = fmap UUID.toText nextRandom
