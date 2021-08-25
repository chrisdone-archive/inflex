{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections, TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

-- | Document loading spec.

module DocumentSpec where

import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord
import qualified Data.Set as Set
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
import           Inflex.Stepper hiding (match)
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator
import           Match
import qualified RIO
import           Test.Hspec

defaultDocument' :: Toposorted (Named (Either LoadError (IsResolved (Expression Resolved)))) -> IO (Toposorted (Named (Either LoadError Cell)))
defaultDocument' = RIO.runRIO DefaulterReader {} . defaultDocument

loadDocument' :: [Named Text] -> IO (Toposorted (Named (Either LoadError (IsResolved (Expression Resolved)))))
loadDocument' = RIO.runRIO DocumentReader {glogfunc = mempty} . loadDocument

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
                      {dependencies = mempty,  uuid = Uuid u1
                      , name = "x"
                      , thing = "@uuid:" <> u1
                      , order = 0
                      , code = "x"
                      , sourceHash = HashNotKnownYet
                      }
                  ]
              (evalDocument' (evalEnvironment loaded)) (defaultDocument' loaded))
          [ Named
              {dependencies = Set.fromList [Uuid u1],  uuid = (Uuid u1)
              , name = "x"
              , thing = Left (CycleError [Uuid u1])
              , order = 0
              , code = "x"
              , sourceHash = HashNotKnownYet
              }
          ])
  it
    "x = y"
    (do u1 <- nextRandom'
        u2 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      {dependencies = mempty,  uuid = Uuid u1
                      , name = "x"
                      , thing = "@uuid:" <> u2
                      , code = "y"
                      , order = 0
                      , sourceHash = HashNotKnownYet
                      }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              {dependencies = Set.fromList [Uuid u2],  uuid = (Uuid u1)
              , name = "x"
              , thing =
                  Left
                    (LoadGenerateError
                       (FillErrors (MissingGlobalUuid emptyFillerEnv (Uuid u2) :| [])))
              , code = "y"
              , order = 0
              , sourceHash = HashNotKnownYet
              }
          ])
  it
    "y = 1; x = y y"
    (do u1 <- nextRandom'
        u2 <- pure "85cbcc37-0c41-4871-a66a-31390a3ef391"
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      {dependencies = mempty,  uuid = Uuid u1
                      , name = "x"
                      , thing = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391)"
                      , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391)"
                      , order = 0
                      , sourceHash = HashNotKnownYet
                      }
                  , Named
                      {dependencies = mempty,  uuid = Uuid u2
                      , name = "y"
                      , thing = "1"
                      , code = "1"
                      , order = 1
                      , sourceHash = HashNotKnownYet
                      }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              {dependencies = Set.fromList [Uuid u2],  uuid = Uuid u1
              , name = "x"
              , order = 0
              , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391)"
              , sourceHash = HashNotKnownYet
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
              {dependencies = mempty,  uuid = Uuid u2
              , sourceHash = HashKnown $$("496cc8e18513539a6614899b2d949ea22aef1fc91bc944ceed35467b4c5abc5919c40cad381802c9e9b70be434c049e72070959d99c823399396d0993a340d7b")
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
           {dependencies = mempty,  uuid = Uuid u1
           , name = "x"
           , order = 0
           , sourceHash = HashNotKnownYet
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
    (do u1 <- pure "85cbaaaa-0c41-4871-a66a-aaaa0a3ef391"
        u2 <- pure "85cbcc37-0c41-4871-a66a-31390a3ef391"
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { dependencies = mempty
                      , uuid = Uuid u1
                      , name = "x"
                      , thing = "1"
                      , code = "1"
                      , order = 0
                      , sourceHash = HashNotKnownYet
                      }
                  , let code = "@uuid:" <> u1
                     in Named
                          { dependencies = Set.fromList [Uuid u1]
                          , uuid = Uuid u2
                          , name = "y"
                          , thing = code
                          , code = code
                          , order = 1
                          , sourceHash = HashNotKnownYet
                          }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { dependencies = mempty
              , uuid = Uuid u1
              , name = "x"
              , order = 0
              , code = "1"
              , sourceHash =
                  HashKnown $$("496cc8e18513539a6614899b2d949ea22aef1fc91bc944ceed35467b4c5abc5919c40cad381802c9e9b70be434c049e72070959d99c823399396d0993a340d7b")
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
              { dependencies =
                  Set.fromList [Uuid "85cbaaaa-0c41-4871-a66a-aaaa0a3ef391"]
              , uuid = Uuid u2
              , sourceHash =
                  HashKnown $$("f488895acc364d9fc7c930adf83f5bd84674592d73d3be734f6b1e8bc8782f09efc144f2f90cdb44d46cdfe713be1cd02d818c7f6b7bea5a4054efef312246ef")
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
    (do u1 <- pure "85cbaaaa-0c41-4871-a66a-aaaa0a3ef391"
        u2 <- pure "85cbcc37-0c41-4871-a66a-31390a3ef391"
        u3 <- pure "12cbcc37-0c41-4871-a66a-31390a3ef666"
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { dependencies = mempty
                      , uuid = Uuid u1
                      , sourceHash = HashNotKnownYet
                      , name = "x"
                      , thing = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391 + 2"
                      , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391 + 2"
                      , order = 0
                      }
                  , Named
                      { dependencies = mempty
                      , uuid = Uuid u2
                      , sourceHash = HashNotKnownYet
                      , name = "y"
                      , thing =
                          "@uuid:12cbcc37-0c41-4871-a66a-31390a3ef666 * 3.1"
                      , code =
                          "@uuid:12cbcc37-0c41-4871-a66a-31390a3ef666 * 3.1"
                      , order = 1
                      }
                  , Named
                      { dependencies = mempty
                      , uuid = Uuid u3
                      , sourceHash = HashNotKnownYet
                      , name = "z"
                      , thing = "2"
                      , code = "2"
                      , order = 2
                      }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { dependencies =
                  Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"]
              , uuid = Uuid u1
              , sourceHash =
                  HashKnown $$("02fbb41a6ba5518920be928053205ba21d9db3cf29d260c99304e0784814dd730ad7b3965888e6fa72617372ab2b8e7aa5f1e14f364275308b59afb5d2a5caa6")
              , name = "x"
              , order = 0
              , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391 + 2"
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
              { dependencies =
                  Set.fromList [Uuid "12cbcc37-0c41-4871-a66a-31390a3ef666"]
              , uuid = Uuid u2
              , sourceHash =
                  HashKnown $$("0e7d48f450dfb84239d528219e711aef445452adb190a5ddc216a0514cd0685edaff0bd9378575ebad34f58042ac19ded073c97ab04ac5a57753b006a86b116d")
              , name = "y"
              , order = 1
              , code = "@uuid:12cbcc37-0c41-4871-a66a-31390a3ef666 * 3.1"
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
              { dependencies = mempty
              , uuid = Uuid u3
              , sourceHash =
                  HashKnown $$("28ef94662cf6bf410f6a5b8a207a89c914d300fd804a44442637eca15a88df1af4d77407a8c211db0bcf237fcf221b274b69fb449a629cd12acc1ce8dda66fed")
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
    (do u1 <- pure "85cbcc37-0c41-4871-a66a-31390a3ef391"
        u2 <- nextRandom'
        u3 <- nextRandom'
        shouldReturn
          (do loaded <-
                loadDocument'
                  [ Named
                      { dependencies = mempty
                      , uuid = Uuid u1
                      , sourceHash = HashNotKnownYet
                      , name = "double"
                      , thing = "x: x * 2"
                      , code = "x: x * 2"
                      , order = 0
                      }
                  , Named
                      { dependencies = mempty
                      , uuid = Uuid u2
                      , sourceHash = HashNotKnownYet
                      , name = "a"
                      , thing = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(1)"
                      , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(1)"
                      , order = 1
                      }
                  , Named
                      { dependencies = mempty
                      , uuid = Uuid u3
                      , sourceHash = HashNotKnownYet
                      , name = "b"
                      , thing =
                          "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(2.2)"
                      , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(2.2)"
                      , order = 2
                      }
                  ]
              evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              { dependencies = mempty
              , uuid = Uuid u1
              , sourceHash =
                  HashKnown $$("81cf95480a9cfab96a11cbe687a56a38af3b018ded75aaf1c7933baf2ee4e291f39aacc454f124171d5e7293ba0f3778cf37d7742e0606b784f729e4009f5686")
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
                                            , style = ImplicitApply
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
                                            , style = OverloadedApply
                                            , function =
                                                ApplyExpression
                                                  (Apply
                                                     { location =
                                                         ImplicitlyApplicationOn
                                                           BuiltIn
                                                     , style = ImplicitApply
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
              { dependencies =
                  Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"]
              , uuid = Uuid u2
              , sourceHash =
                  HashKnown $$("e9532ff1b95318b17bff6d3c1a48eb54ed6a5fe353ec788f8488438f688a8a508f2c09d9a582e94903c5024aea77c8013c4293638e3722b85b6eca70216a7d3e")
              , name = "a"
              , order = 1
              , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(1)"
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
              { dependencies =
                  Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"]
              , uuid = Uuid u3
              , sourceHash =
                  HashKnown $$("afcd6a9e22c37eb56b024c2dcd9da567da8da174d3c91fb68d52bc042d637c3ab59d00b19458b9482d995c4158d239fb1207842425f056e7a9c0dfefb128378d")
              , name = "b"
              , order = 2
              , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391(2.2)"
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
           {dependencies = mempty,  uuid = Uuid u1
           , sourceHash = HashKnown $$("b3ee6b4afdc8e62a0b4e3b30c59132c0b5d6cb72cf8280becc96bf2d07181e756c7a5276933891de0de96f04f68059828089597588ef0669bd6ff3b51aff2b15")
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
                                   SteppedCursor
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
  eval_it_with_uuids
    "Arith referencing a unary record is fine"
    [ (Just "85cbcc37-0c41-4871-a66a-31390a3ef391", ("x", "{a:666}"))
    , (Nothing, ("y", "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391.a * 2"))
    ]
    (\[u1, u2] ->
       [ Named
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("c03d7e305d4ebcc94cc810a45e79f3c6d95e4fba91a0ac4afca17ebe3d61c42a8939fadffddf59e17dd9c8075dc6ca95cb9371f4a892fc068244d983d8c749c5")
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
                                   SteppedCursor
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
           { dependencies =
               Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"]
           , uuid = Uuid u2
           , sourceHash =
               HashKnown $$("3bf9935908221761972174126c61d1d8868d39138e19b817e59d71918beb8b053dc5a97c63d5d3387ee50244b8a3de127180bd1589b0c604be5f8c430c560795")
           , name = "y"
           , order = 1
           , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391.a * 2"
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
  eval_it_with_uuids
    "Referencing a single field from a 2-ary record is fine (without type sig)"
    [ (Just "85cbcc37-0c41-4871-a66a-31390a3ef391", ("x", "{a:1, b:8}"))
    , (Nothing, ("y", "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391.a"))
    ]
    (\[u1, u2] ->
       [ Named
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("8a08f41e772aad71028d7fabc55261c43a68af8e756ec887305a85ad3bb47bb33412793579411130cf34426b4aa2135e4cd915e57214cbb4fb3273521a851a35")
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
                                   SteppedCursor
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
                                   SteppedCursor
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
           { dependencies =
               Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"]
           , uuid = Uuid u2
           , sourceHash =
               HashKnown $$("f526912dc66236a4336a699a2649bae27e3705d022a881a3487ecb026a532ca622f3b5a4d89fdf66f5ec60cde30755dd0aed444ff75aa042e9babe45390b746c")
           , name = "y"
           , order = 1
           , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391.a"
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
  eval_it_with_uuids
    "Referencing a single field from a 2-ary record is fine (with type sig)"
    [ ( Just "85cbcc37-0c41-4871-a66a-31390a3ef391"
      , ("x", "{a:1, b:8 :: Integer}"))
    , (Nothing, ("y", "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391.a"))
    ]
    (\[u1, u2] ->
       [ Named
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("99dfe6586d94f0c9264c8d8c5cdba1eff050940d76e1e0e3cead4bd02ef2c84d6485792e02526328e33cba7cde1a3d6f975943947dd758bd12745bbd1ba88d6b")
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
                                   SteppedCursor
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
                                   SteppedCursor
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
           { dependencies =
               Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"]
           , uuid = Uuid u2
           , sourceHash =
               HashKnown $$("528d5b6c0dde5ef28d838c1b5673292e9707b8b9b6ea219cb350f3359ad2b4fac5b1645aa846dbad0a55cfe14359920b77bcd9c3eeeb6a60451bbfe33e523e93")
           , name = "y"
           , order = 1
           , code = "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391.a"
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
  eval_it_with_uuids
    "Defaulting for a verbatim copy of a record is also fine:"
    [ (Just "85cbcc37-0c41-4871-a66a-31390a3ef391", ("x", "{a:1, b:8}"))
    , (Nothing, ("y", "@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391"))
    ]
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
                              SteppedCursor
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
                              SteppedCursor
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
               {dependencies = mempty,  uuid = Uuid u1
               , sourceHash = HashKnown $$("8a08f41e772aad71028d7fabc55261c43a68af8e756ec887305a85ad3bb47bb33412793579411130cf34426b4aa2135e4cd915e57214cbb4fb3273521a851a35")
               , name = "x"
               , order = 0
               , code = "{a:1, b:8}"
               , thing = Right record
               }
           , Named
               {dependencies = Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"],  uuid = Uuid u2
               , sourceHash = HashKnown $$("3fae5144234757c308da78aee3358417042870e7306914a99eabf535b1ae3fdc26bbcf8e6ba31c67579daf25d91fe70998d58b57b255c9b7c6685e7f1ce4bb96")
               , name = "y"
               , order = 1
               , code = "@uuid:" <> u1
               , thing = Right record
               }
           ])
  eval_it
    "Immediate access of a record's field is fine"
    [("x", "{a:1}.a")]
    (\[u1] ->
       [ Named
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("8d1c99250adf37964a3d85bac79d7eca856bc89b2ddae025b5f36f875e08185458eff8dabc4d35ab4e7d89f48deef1aae6615f37c481ba9500e0cca9c08226a2")
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
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("b534f402e5b8308635a60211984249c272c131e96c2d8ccedd67acff4a42b3555b6ddc0988d1c57fe5a061195fb2e655d06c2638df7295bf758ffdff4a909f8f")
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
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("1a1cbd8626272a46060d49c63ab803ef9d4a471a0746f3a110eefb912d669df7c2b2b7ef8bc9cc6713e0617db5acfec52c98fa35fedb4092d0f46fd82242f6fb")
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
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("caf4c09bedce9a0e104917cff2857e0854b7303fb37ed8571f1c2a720f30a83fc219042ae500b71569096acd69965c4ccb10fcc7cf53fd715568ae8ccf3bcbba")
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
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("3c7010946744873af91f97276c86614321f43cf1589fdd6d87814d82d801aa74a016ad7dd5c7468841d148d1a0c2f8b134e71fc8b512abd613139d70b793248a")
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
           { dependencies = mempty
           , uuid = Uuid u1
           , sourceHash =
               HashKnown $$("07fea6cc46f28f88ce0bcff56a56309e4c933598a55088478d1aa6f28ad1e17d7aa3017cc45694dea4cd8b76898832f6d03576d48f39a4c3cd67a4efdb41276b")
           , name = "x"
           , order = 0
           , code = "[1.0,2*3]"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { form = Evaluated
                       , expressions =
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
  monzo_list

mapfunc :: SpecWith ()
mapfunc =
  eval_it
    "@prim:array_map(x:x*2,[3])"
    [("x", "(@prim:array_map(x:x*2))([3])")]
    (\[u1] ->
       [ Named
           {dependencies = mempty,  uuid = Uuid u1
           , sourceHash = HashKnown $$("0d6bf11f785382417c095da37c0a9ef582ce210789af2e6d890957077ff74ad26852ddd560df767759438f8b3fe12f077326b16a9f8bcda408af0792fc260257")
           , name = "x"
           , order = 0
           , code = "(@prim:array_map(x:x*2))([3])"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { form = Evaluated,
                         expressions =
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
                       , location = SteppedCursor
                       }))
           }
       ])

table_map_defaulting :: SpecWith ()
table_map_defaulting =
  eval_it_with_uuids
    "t = [{x:3,y:2.0}]; k = map(r:r.x*r.y,t)"
    [ (Just "85cbcc37-0c41-4871-a66a-31390a3ef391", ("t", "[{x:3,y:2.0}]"))
    , (Nothing, ("k", "@prim:array_map(r:r.x*r.y, @uuid:85cbcc37-0c41-4871-a66a-31390a3ef391)"))
    ]
    (\[u1, u2] ->
       [ Named
           {dependencies = mempty,  uuid = Uuid u1
           , sourceHash = HashKnown $$("ceb16ec7295de1dfee5b07a2ca2e16b720f53bf4e51883a09d7772dfd3c14beb0257a0660194e052e65fe0b6e85f759f9ce8f91319275cbbfeba3a9f3616660f")
           , name = "t"
           , order = 0
           , code = "[{x:3,y:2.0}]"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { form = Evaluated,
                         expressions =
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
                                                SteppedCursor
                                            }
                                        , FieldE
                                            { name =
                                                FieldName {unFieldName = "y"}
                                            , expression =
                                                LiteralExpression
                                                  (NumberLiteral
                                                     (Number
                                                        { location = BuiltIn
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
                                                SteppedCursor
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
           {dependencies = Set.fromList [Uuid "85cbcc37-0c41-4871-a66a-31390a3ef391"],  uuid = Uuid u2
           , sourceHash = HashKnown $$("95f7e58a79f26392d5a20f2bd5f3aaf83e6fe991585e3c5d3c3da553505c8a4324378ab4892752468560e04ae2c147e37b36c7b21d4d505f81cb011e2e72f910")
           , name = "k"
           , order = 1
           , code = "@prim:array_map(r:r.x*r.y, @uuid:85cbcc37-0c41-4871-a66a-31390a3ef391)"
           , thing =
               Right
                 (ArrayExpression
                    (Array
                       { form = Evaluated, expressions =
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
                       , location = SteppedCursor
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
                    {dependencies = mempty,  uuid = Uuid u1
                    , sourceHash = HashNotKnownYet
                    , name = "x"
                    , thing = "193 :: Decimal 2"
                    , code = "193 :: Decimal 2"
                    , order = 0
                    }
                , Named
                    {dependencies = mempty,  uuid = Uuid u2
                    , sourceHash = HashNotKnownYet
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
                {dependencies = mempty,  uuid = Uuid u1
                , sourceHash = HashNotKnownYet
                , name = ""
                , thing = "193"
                , code = "193"
                , order = 0
                }
            , Named
                {dependencies = mempty,  uuid = Uuid u2
                , sourceHash = HashNotKnownYet
                , name = ""
                , thing = "2+3"
                , code = "2+3"
                , order = 1
                }
            ]
        shouldReturn
          (evalDocument' (evalEnvironment loaded) (defaultDocument' loaded))
          [ Named
              {dependencies = mempty,  uuid = Uuid u1
              , sourceHash = HashKnown $$("9f3369ce5041e354f3607cae9467e3b1359780d3097dff8dad5fb17879facf914c76a60cbc775d9099511ffdfe51600e33ab3aceae0b546d3bcecbe1b7d4ec88")
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
               {dependencies = mempty,  uuid = Uuid u2
               , sourceHash = HashKnown $$("acfaa6e22875ce6400223bb1d09105c297ecb53668761d3074aebd7071d86e8eebf5c415cc278debb6b64b85b74b494f160613e3af009ca2455c7997935b3868")
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

monzo_list :: SpecWith ()
monzo_list = do
  eval_it_match
    "monzo array, regression test"
    [ ( Just "85cbcc37-0c41-4871-a66a-31390a3ef391"
      , ( "t"
        , "[{\"Transaction ID\": \"tx_00009hQgBGyAJ3lH8TL1XO\", \"Date\": \"03/04/2019\", \"Time\": \"11:47:59\", \"Type\": \"Card payment\", \"Name\": #ok(\"Ukiyo Republic Ltd\"), \"Emoji\": #none, \"Category\": #ok(\"Eating out\"), \"Amount\": \"-13.05\", \"Currency\": \"GBP\", \"Local amount\": \"-13.05\", \"Local currency\": \"GBP\", \"Notes and #tags\": #none, \"Address\": #ok(\"20-22 Wenlock Road\"), \"Receipt\": \"\", \"Description\": #ok(\"IZ *UKIYO REPUBLIC LTD London        GBR\"), \"Category split\": \"\", \"Money Out\": #ok(\"-13.05\"), \"Money In\": #none}] :: [{\"Transaction ID\":Text, \"Date\":Text, \"Time\":Text, \"Type\":Text, \"Name\":<\"ok\":Text, \"none\":{}|_>, \"Emoji\":<\"ok\":Text, \"none\":{}|_>, \"Category\":<\"ok\":Text, \"none\":{}|_>, \"Amount\":Text, \"Currency\":Text, \"Local amount\":Text, \"Local currency\":Text, \"Notes and #tags\":<\"ok\":Text, \"none\":{}|_>, \"Address\":<\"ok\":Text, \"none\":{}|_>, \"Receipt\":Text, \"Description\":<\"ok\":Text, \"none\":{}|_>, \"Category split\":Text, \"Money Out\":<\"ok\":Text, \"none\":{}|_>, \"Money In\":<\"ok\":Decimal 2, \"none\":{}|_>}]"))
    , ( Nothing
      , ("k", "@prim:array_length(@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391)"))
    ]
    (\[u1, u2] r -> do
       shouldSatisfy
         r
         $(match
             [|[ Named {uuid = Uuid u1, thing = Right _}
               , Named {uuid = Uuid u2, thing = Right _}
               ]|]))
    (maybe nextRandom' pure)
    (pure ())
  eval_it_match
    "core issue - length funcion had wrong signature"
    [ ( Just "85cbcc37-0c41-4871-a66a-31390a3ef391"
      , ("t", "[\"x\",\"y\",\"z\"]"))
    , ( Nothing
      , ("k", "@prim:array_length(@uuid:85cbcc37-0c41-4871-a66a-31390a3ef391)"))
    ]
    (\[u1, u2] r -> do
       shouldSatisfy
         r
         $(match
             [|[ Named {uuid = Uuid u1, thing = Right _}
               , Named
                   { uuid = Uuid u2
                   , thing =
                       Right
                         (LiteralExpression
                            (NumberLiteral (Number {number = IntegerNumber 3})))
                   }
               ]|]))
    (maybe nextRandom' pure)
    (pure ())

--------------------------------------------------------------------------------
-- Helpers

eval_it ::
     String -> [(Text, Text)]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))])
  -> SpecWith ()
eval_it desc xs result = eval_it_with desc (map (Nothing, ) xs) result (maybe nextRandom' pure) (pure ())

eval_it_with_uuids ::
     String
  -> [(Maybe Text, (Text, Text))]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))])
  -> SpecWith ()
eval_it_with_uuids desc xs result =
  eval_it_with
    desc
    xs
    result
    (maybe nextRandom' pure)
    (pure ())

eval_it_pending ::
     String -> [(Text, Text)]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))])
  -> String
  -> SpecWith ()
eval_it_pending desc xs result t = eval_it_with desc (map (Nothing, ) xs) result (maybe nextRandom' pure) (pendingWith t)

eval_it_with ::
     String
  -> [(Maybe Text, (Text, Text))]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))])
  -> (Maybe Text -> IO Text)
  -> IO ()
  -> SpecWith ()
eval_it_with desc xs result next io =
  it
    (desc <> ": " <>
     intercalate
       "; "
       (map (\(_, (name, val)) -> T.unpack name <> " = " <> T.unpack val) xs))
    (do io
        xs' <-
          mapM
            (\(u0, x) -> do
               u <- next u0
               pure (u, x))
            xs
        shouldReturn
          (do loaded <-
                loadDocument'
                  (zipWith
                     (\i (uuid, (name, thing)) ->
                        Named
                          {dependencies = mempty,  uuid = Uuid uuid
                          , name
                          , thing
                          , code = thing
                          , order = i
                          , sourceHash = HashNotKnownYet
                          })
                     [0 ..]
                     xs')
              evalDocument'
                (evalEnvironment loaded)
                (RIO.runRIO DefaulterReader (defaultDocument loaded)))
          (result (map fst xs')))

eval_it_match ::
     String
  -> [(Maybe Text, (Text, Text))]
  -> ([Text] -> [Named (Either LoadError (Expression Resolved))] -> IO ())
  -> (Maybe Text -> IO Text)
  -> IO ()
  -> SpecWith ()
eval_it_match desc xs should next io =
  it
    (desc <> ": " <>
     intercalate
       "; "
       (map (\(_, (name, val)) -> T.unpack name <> " = " <> T.unpack val) xs))
    (do io
        xs' <-
          mapM
            (\(u0, x) -> do
               u <- next u0
               pure (u, x))
            xs
        loaded <-
          loadDocument'
            (zipWith
               (\i (uuid, (name, thing)) ->
                  Named {dependencies = mempty, uuid = Uuid uuid, name, thing, code = thing, order = i, sourceHash = HashNotKnownYet})
               [0 ..]
               xs')
        res <-
          evalDocument'
            (evalEnvironment loaded)
            (RIO.runRIO DefaulterReader (defaultDocument loaded))
        should (map fst xs') res)

nextRandom' :: IO Text
nextRandom' = fmap UUID.toText nextRandom
