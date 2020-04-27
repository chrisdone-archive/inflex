{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

import "monad-logger"  Control.Monad.Logger
import "inflex-engine" Control.Monad.Supply
import                 Control.Monad.Writer
import                 Data.Bifunctor
import                 Data.Map.Strict (Map)
import qualified       Data.Map.Strict as M
import                 Data.Maybe
import                 Data.Text (Text)
import qualified       Data.Text as T
import                 Data.Vector (Vector)
import qualified       Data.Vector as V
import                 Debug.Trace
import                 Duet.Infer
import                 Duet.Parser
import                 Duet.Printer
import                 Duet.Simple
import                 Duet.Types
import qualified       Language.PureScript.CST as CST
import                 Test.Hspec
import                 Test.Validity (forAllUnchecked)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "Parser" parserTests
  describe "Inference" inferenceTests
  describe
    "Compilation"
    (do it
          "Basic compile and run constant"
          (shouldBe
             (first
                (const ())
                (runNoLoggingT
                   ((evalSupplyT
                       (do decls <- parseText "test" "main = 1"
                           (binds, ctx) <- createContext decls
                           things <-
                             execWriterT
                               (runStepper
                                  100
                                  ctx
                                  (fmap (fmap typeSignatureA) binds)
                                  "main")
                           pure things)
                       [1 ..]))))
             (Right [LiteralExpression () (IntegerLiteral 1)]))
        it
          "Basic compile and run constant lambda"
          (shouldBe
             (first
                (const ())
                (runNoLoggingT
                   ((evalSupplyT
                       (do decls <- parseText "test" "main = (\\x -> x) 1"
                           (binds, ctx) <- createContext decls
                           things <-
                             execWriterT
                               (runStepper
                                  100
                                  ctx
                                  (fmap (fmap typeSignatureA) binds)
                                  "main")
                           pure things)
                       [1 ..]))))
             (Right
                [ ApplicationExpression
                    ()
                    (LambdaExpression
                       ()
                       (Alternative
                          { alternativeLabel = ()
                          , alternativePatterns =
                              [VariablePattern () (ValueName 50 "x")]
                          , alternativeExpression =
                              VariableExpression () (ValueName 50 "x")
                          }))
                    (LiteralExpression () (IntegerLiteral 1))
                , LiteralExpression () (IntegerLiteral 1)
                ]))
        it
          "Seq"
          (shouldBe
             (second
                last
                (first
                   (const ())
                   (runNoLoggingT
                      ((evalSupplyT
                          (do decls <-
                                parseText
                                  "test"
                                  "seq =\n\
                                 \  \\x y ->\n\
                                 \    case x of\n\
                                 \      !_ -> y\n\
                                 \loop = loop\n\
                                 \main = seq loop 1"
                              (binds, ctx) <- createContext decls
                              things <-
                                execWriterT
                                  (runStepper
                                     100
                                     ctx
                                     (fmap (fmap typeSignatureA) binds)
                                     "main")
                              pure things)
                          [1 ..])))))
             (Right
                (CaseExpression
                   ()
                   (VariableExpression () (ValueName 50 "loop"))
                   [ CaseAlt
                       { caseAltLabel = ()
                       , caseAltPattern = BangPattern (WildcardPattern () "_")
                       , caseAltExpression =
                           LiteralExpression () (IntegerLiteral 1)
                       }
                   ])))
        describe
          "Array"
          (do it
                "Inferred type Array a"
                (shouldBe
                   (second
                      (map (map implicitlyTypedBindingLabel) .
                       bindGroupImplicitlyTypedBindings)
                      (first
                         show
                         (runNoLoggingT
                            (evalSupplyT
                               (do decls <- parseText "test" "main = []"
                                   (binds, _ctx) <- createContext decls
                                   pure (head binds))
                               [1 ..]))))
                   (Right
                      [ [ TypeSignature
                            { typeSignatureA =
                                Location
                                  { locationStartLine = 1
                                  , locationStartColumn = 1
                                  , locationEndLine = 1
                                  , locationEndColumn = 5
                                  }
                            , typeSignatureScheme =
                                Forall
                                  [ TypeVariable
                                      { typeVariableIdentifier = ForallName 1
                                      , typeVariableKind = StarKind
                                      }
                                  ]
                                  (Qualified
                                     { qualifiedPredicates = []
                                     , qualifiedType =
                                         ApplicationType
                                           (ConstructorType
                                              (TypeConstructor
                                                 { typeConstructorIdentifier =
                                                     TypeName 1 "Array"
                                                 , typeConstructorKind =
                                                     FunctionKind
                                                       StarKind
                                                       StarKind
                                                 }))
                                           (VariableType
                                              (TypeVariable
                                                 { typeVariableIdentifier =
                                                     ForallName 1
                                                 , typeVariableKind = StarKind
                                                 }))
                                     })
                            }
                        ]
                      ]))
              it
                "Inferred type (Array Integer)"
                (shouldBe
                   (second
                      (map (map implicitlyTypedBindingLabel) .
                       bindGroupImplicitlyTypedBindings)
                      (first
                         show
                         (runNoLoggingT
                            (evalSupplyT
                               (do decls <- parseText "test" "main = [1]"
                                   (binds, _ctx) <- createContext decls
                                   pure (head binds))
                               [1 ..]))))
                   (Right
                      [ [ (TypeSignature
                             { typeSignatureA =
                                 Location
                                   { locationStartLine = 1
                                   , locationStartColumn = 1
                                   , locationEndLine = 1
                                   , locationEndColumn = 5
                                   }
                             , typeSignatureScheme =
                                 Forall
                                   []
                                   (Qualified
                                      { qualifiedPredicates = []
                                      , qualifiedType =
                                          ApplicationType
                                            (ConstructorType
                                               (TypeConstructor
                                                  { typeConstructorIdentifier =
                                                      TypeName 1 "Array"
                                                  , typeConstructorKind =
                                                      FunctionKind
                                                        StarKind
                                                        StarKind
                                                  }))
                                            (ConstructorType
                                               (TypeConstructor
                                                  { typeConstructorIdentifier =
                                                      TypeName 5 "Integer"
                                                  , typeConstructorKind =
                                                      StarKind
                                                  }))
                                      })
                             })
                        ]
                      ]))
              it
                "Inferred type (Array (Array Integer))"
                (shouldBe
                   (second
                      (map (map implicitlyTypedBindingLabel) .
                       bindGroupImplicitlyTypedBindings)
                      (first
                         show
                         (runNoLoggingT
                            (evalSupplyT
                               (do decls <- parseText "test" "main = [[1]]"
                                   (binds, _ctx) <- createContext decls
                                   pure (head binds))
                               [1 ..]))))
                   (Right
                      [ [ TypeSignature
                            { typeSignatureA =
                                Location
                                  { locationStartLine = 1
                                  , locationStartColumn = 1
                                  , locationEndLine = 1
                                  , locationEndColumn = 5
                                  }
                            , typeSignatureScheme =
                                Forall
                                  []
                                  (Qualified
                                     { qualifiedPredicates = []
                                     , qualifiedType =
                                         ApplicationType
                                           (ConstructorType
                                              (TypeConstructor
                                                 { typeConstructorIdentifier =
                                                     TypeName 1 "Array"
                                                 , typeConstructorKind =
                                                     FunctionKind
                                                       StarKind
                                                       StarKind
                                                 }))
                                           (ApplicationType
                                              (ConstructorType
                                                 (TypeConstructor
                                                    { typeConstructorIdentifier =
                                                        TypeName 1 "Array"
                                                    , typeConstructorKind =
                                                        FunctionKind
                                                          StarKind
                                                          StarKind
                                                    }))
                                              (ConstructorType
                                                 (TypeConstructor
                                                    { typeConstructorIdentifier =
                                                        TypeName 5 "Integer"
                                                    , typeConstructorKind =
                                                        StarKind
                                                    })))
                                     })
                            }
                        ]
                      ]))
              it
                "Explicit type Array Integer"
                (shouldBe
                   (first
                      show
                      (runNoLoggingT
                         ((evalSupplyT
                             (do decls <-
                                   parseText
                                     "test"
                                     "main :: Array Integer\nmain = [1]"
                                 (binds, ctx) <- createContext decls
                                 things <-
                                   execWriterT
                                     (runStepper
                                        100
                                        ctx
                                        (fmap (fmap typeSignatureA) binds)
                                        "main")
                                 pure things)
                             [1 ..]))))
                   (Right
                      [ ArrayExpression
                          ()
                          (V.fromList [LiteralExpression () (IntegerLiteral 1)])
                      ]))
              it
                "Evaluate array"
                (shouldBe
                   (first
                      (const ())
                      (runNoLoggingT
                         (evalSupplyT
                            (do decls <- parseText "test" "main = [2 * 3, 5]"
                                (binds, ctx) <- createContext decls
                                things <-
                                  execWriterT
                                    (runStepper
                                       3
                                       ctx
                                       (fmap (fmap typeSignatureA) binds)
                                       "main")
                                pure things)
                            [1 ..])))
                   (Right
                      [ ArrayExpression
                          ()
                          (V.fromList
                             [ InfixExpression
                                 ()
                                 (LiteralExpression () (IntegerLiteral 2))
                                 ( "*"
                                 , ApplicationExpression
                                     ()
                                     (VariableExpression
                                        ()
                                        (MethodName 12 "times"))
                                     (VariableExpression
                                        ()
                                        (DictName 37 "$dictNum_Integer")))
                                 (LiteralExpression () (IntegerLiteral 3))
                             , LiteralExpression () (IntegerLiteral 5)
                             ])
                      , ArrayExpression
                          ()
                          (V.fromList
                             [ LiteralExpression () (IntegerLiteral 6)
                             , LiteralExpression () (IntegerLiteral 5)
                             ])
                      ]))))

parserTests =
  it
    "Records"
    (shouldReturn
       (parseText "" "x = {x: 1, y: 2 * 3, z: 4}")
       [ BindDecl
           (Location
              { locationStartLine = 1
              , locationStartColumn = 1
              , locationEndLine = 1
              , locationEndColumn = 2
              })
           (ImplicitBinding
              (ImplicitlyTypedBinding
                 { implicitlyTypedBindingLabel =
                     Location
                       { locationStartLine = 1
                       , locationStartColumn = 1
                       , locationEndLine = 1
                       , locationEndColumn = 2
                       }
                 , implicitlyTypedBindingId =
                     ( Identifier {identifierString = "x"}
                     , Location
                         { locationStartLine = 1
                         , locationStartColumn = 1
                         , locationEndLine = 1
                         , locationEndColumn = 2
                         })
                 , implicitlyTypedBindingAlternatives =
                     [ Alternative
                         { alternativeLabel =
                             Location
                               { locationStartLine = 1
                               , locationStartColumn = 1
                               , locationEndLine = 1
                               , locationEndColumn = 2
                               }
                         , alternativePatterns = []
                         , alternativeExpression =
                             RowExpression
                               (Location
                                  { locationStartLine = 1
                                  , locationStartColumn = 5
                                  , locationEndLine = 1
                                  , locationEndColumn = 6
                                  })
                               (M.fromList
                                  [ ( Identifier {identifierString = "x"}
                                    , LiteralExpression
                                        (Location
                                           { locationStartLine = 1
                                           , locationStartColumn = 9
                                           , locationEndLine = 1
                                           , locationEndColumn = 10
                                           })
                                        (IntegerLiteral 1))
                                  , ( Identifier {identifierString = "y"}
                                    , InfixExpression
                                        (Location
                                           { locationStartLine = 0
                                           , locationStartColumn = 0
                                           , locationEndLine = 0
                                           , locationEndColumn = 0
                                           })
                                        (LiteralExpression
                                           (Location
                                              { locationStartLine = 1
                                              , locationStartColumn = 15
                                              , locationEndLine = 1
                                              , locationEndColumn = 16
                                              })
                                           (IntegerLiteral 2))
                                        ( "*"
                                        , VariableExpression
                                            (Location
                                               { locationStartLine = 0
                                               , locationStartColumn = 0
                                               , locationEndLine = 0
                                               , locationEndColumn = 0
                                               })
                                            (Identifier {identifierString = "*"}))
                                        (LiteralExpression
                                           (Location
                                              { locationStartLine = 1
                                              , locationStartColumn = 19
                                              , locationEndLine = 1
                                              , locationEndColumn = 20
                                              })
                                           (IntegerLiteral 3)))
                                  , ( Identifier {identifierString = "z"}
                                    , LiteralExpression
                                        (Location
                                           { locationStartLine = 1
                                           , locationStartColumn = 25
                                           , locationEndLine = 1
                                           , locationEndColumn = 26
                                           })
                                        (IntegerLiteral 4))
                                  ])
                         }
                     ]
                 }))
       ])

inferenceTests =
  it
    "Basic compile and run constant"
    (shouldBe
       (first
          show
          (runNoLoggingT
             ((evalSupplyT
                 (do decls <- parseText "test" "r = {x: 2, y: 3 * 6}"
                     (binds, ctx) <- createContext decls
                     pure (take 1 binds))
                 [1 ..]))))
       (Right
          [ BindGroup
              { bindGroupExplicitlyTypedBindings = []
              , bindGroupImplicitlyTypedBindings =
                  [ [ ImplicitlyTypedBinding
                        { implicitlyTypedBindingLabel =
                            TypeSignature
                              { typeSignatureA =
                                  Location
                                    { locationStartLine = 1
                                    , locationStartColumn = 1
                                    , locationEndLine = 1
                                    , locationEndColumn = 2
                                    }
                              , typeSignatureScheme =
                                  Forall
                                    []
                                    (Qualified
                                       { qualifiedPredicates = []
                                       , qualifiedType =
                                           RowType
                                             (Row
                                                { rowVariable = Nothing
                                                , rowFields =
                                                    [ Field
                                                        { fieldName =
                                                            Identifier
                                                              {identifierString = "x"}
                                                        , fieldType =
                                                            ConstructorType
                                                              (TypeConstructor
                                                                 { typeConstructorIdentifier =
                                                                     TypeName
                                                                       5
                                                                       "Integer"
                                                                 , typeConstructorKind =
                                                                     StarKind
                                                                 })
                                                        }
                                                    , Field
                                                        { fieldName =
                                                            Identifier
                                                              {identifierString = "y"}
                                                        , fieldType =
                                                            ConstructorType
                                                              (TypeConstructor
                                                                 { typeConstructorIdentifier =
                                                                     TypeName
                                                                       5
                                                                       "Integer"
                                                                 , typeConstructorKind =
                                                                     StarKind
                                                                 })
                                                        }
                                                    ]
                                                })
                                       })
                              }
                        , implicitlyTypedBindingId =
                            ( ValueName 49 "r"
                            , TypeSignature
                                { typeSignatureA =
                                    Location
                                      { locationStartLine = 1
                                      , locationStartColumn = 1
                                      , locationEndLine = 1
                                      , locationEndColumn = 2
                                      }
                                , typeSignatureScheme =
                                    Forall
                                      []
                                      (Qualified
                                         { qualifiedPredicates = []
                                         , qualifiedType =
                                             RowType
                                               (Row
                                                  { rowVariable = Nothing
                                                  , rowFields =
                                                      [ Field
                                                          { fieldName =
                                                              Identifier
                                                                {identifierString = "x"}
                                                          , fieldType =
                                                              ConstructorType
                                                                (TypeConstructor
                                                                   { typeConstructorIdentifier =
                                                                       TypeName
                                                                         5
                                                                         "Integer"
                                                                   , typeConstructorKind =
                                                                       StarKind
                                                                   })
                                                          }
                                                      , Field
                                                          { fieldName =
                                                              Identifier
                                                                {identifierString = "y"}
                                                          , fieldType =
                                                              ConstructorType
                                                                (TypeConstructor
                                                                   { typeConstructorIdentifier =
                                                                       TypeName
                                                                         5
                                                                         "Integer"
                                                                   , typeConstructorKind =
                                                                       StarKind
                                                                   })
                                                          }
                                                      ]
                                                  })
                                         })
                                })
                        , implicitlyTypedBindingAlternatives =
                            [ Alternative
                                { alternativeLabel =
                                    TypeSignature
                                      { typeSignatureA =
                                          Location
                                            { locationStartLine = 1
                                            , locationStartColumn = 1
                                            , locationEndLine = 1
                                            , locationEndColumn = 2
                                            }
                                      , typeSignatureScheme =
                                          Forall
                                            []
                                            (Qualified
                                               { qualifiedPredicates =
                                                   [ IsIn
                                                       (ClassName 13 "Num")
                                                       [ ConstructorType
                                                           (TypeConstructor
                                                              { typeConstructorIdentifier =
                                                                  TypeName 5 "Integer"
                                                              , typeConstructorKind =
                                                                  StarKind
                                                              })
                                                       ]
                                                   ]
                                               , qualifiedType =
                                                   RowType
                                                     (Row
                                                        { rowVariable = Nothing
                                                        , rowFields =
                                                            [ Field
                                                                { fieldName =
                                                                    Identifier
                                                                      { identifierString =
                                                                          "x"
                                                                      }
                                                                , fieldType =
                                                                    ConstructorType
                                                                      (TypeConstructor
                                                                         { typeConstructorIdentifier =
                                                                             TypeName
                                                                               5
                                                                               "Integer"
                                                                         , typeConstructorKind =
                                                                             StarKind
                                                                         })
                                                                }
                                                            , Field
                                                                { fieldName =
                                                                    Identifier
                                                                      { identifierString =
                                                                          "y"
                                                                      }
                                                                , fieldType =
                                                                    ConstructorType
                                                                      (TypeConstructor
                                                                         { typeConstructorIdentifier =
                                                                             TypeName
                                                                               5
                                                                               "Integer"
                                                                         , typeConstructorKind =
                                                                             StarKind
                                                                         })
                                                                }
                                                            ]
                                                        })
                                               })
                                      }
                                , alternativePatterns = []
                                , alternativeExpression =
                                    RowExpression
                                      (TypeSignature
                                         { typeSignatureA =
                                             Location
                                               { locationStartLine = 1
                                               , locationStartColumn = 5
                                               , locationEndLine = 1
                                               , locationEndColumn = 6
                                               }
                                         , typeSignatureScheme =
                                             Forall
                                               []
                                               (Qualified
                                                  { qualifiedPredicates = []
                                                  , qualifiedType =
                                                      RowType
                                                        (Row
                                                           { rowVariable = Nothing
                                                           , rowFields =
                                                               [ Field
                                                                   { fieldName =
                                                                       Identifier
                                                                         { identifierString =
                                                                             "x"
                                                                         }
                                                                   , fieldType =
                                                                       ConstructorType
                                                                         (TypeConstructor
                                                                            { typeConstructorIdentifier =
                                                                                TypeName
                                                                                  5
                                                                                  "Integer"
                                                                            , typeConstructorKind =
                                                                                StarKind
                                                                            })
                                                                   }
                                                               , Field
                                                                   { fieldName =
                                                                       Identifier
                                                                         { identifierString =
                                                                             "y"
                                                                         }
                                                                   , fieldType =
                                                                       ConstructorType
                                                                         (TypeConstructor
                                                                            { typeConstructorIdentifier =
                                                                                TypeName
                                                                                  5
                                                                                  "Integer"
                                                                            , typeConstructorKind =
                                                                                StarKind
                                                                            })
                                                                   }
                                                               ]
                                                           })
                                                  })
                                         })
                                      (M.fromList
                                         [ ( Identifier {identifierString = "x"}
                                           , LiteralExpression
                                               (TypeSignature
                                                  { typeSignatureA =
                                                      Location
                                                        { locationStartLine = 1
                                                        , locationStartColumn = 9
                                                        , locationEndLine = 1
                                                        , locationEndColumn = 10
                                                        }
                                                  , typeSignatureScheme =
                                                      Forall
                                                        []
                                                        (Qualified
                                                           { qualifiedPredicates = []
                                                           , qualifiedType =
                                                               ConstructorType
                                                                 (TypeConstructor
                                                                    { typeConstructorIdentifier =
                                                                        TypeName
                                                                          5
                                                                          "Integer"
                                                                    , typeConstructorKind =
                                                                        StarKind
                                                                    })
                                                           })
                                                  })
                                               (IntegerLiteral 2))
                                         , ( Identifier {identifierString = "y"}
                                           , InfixExpression
                                               (TypeSignature
                                                  { typeSignatureA =
                                                      Location
                                                        { locationStartLine = 0
                                                        , locationStartColumn = 0
                                                        , locationEndLine = 0
                                                        , locationEndColumn = 0
                                                        }
                                                  , typeSignatureScheme =
                                                      Forall
                                                        []
                                                        (Qualified
                                                           { qualifiedPredicates =
                                                               [ IsIn
                                                                   (ClassName 13 "Num")
                                                                   [ ConstructorType
                                                                       (TypeConstructor
                                                                          { typeConstructorIdentifier =
                                                                              TypeName
                                                                                5
                                                                                "Integer"
                                                                          , typeConstructorKind =
                                                                              StarKind
                                                                          })
                                                                   ]
                                                               ]
                                                           , qualifiedType =
                                                               ConstructorType
                                                                 (TypeConstructor
                                                                    { typeConstructorIdentifier =
                                                                        TypeName
                                                                          5
                                                                          "Integer"
                                                                    , typeConstructorKind =
                                                                        StarKind
                                                                    })
                                                           })
                                                  })
                                               (LiteralExpression
                                                  (TypeSignature
                                                     { typeSignatureA =
                                                         Location
                                                           { locationStartLine = 1
                                                           , locationStartColumn = 15
                                                           , locationEndLine = 1
                                                           , locationEndColumn = 16
                                                           }
                                                     , typeSignatureScheme =
                                                         Forall
                                                           []
                                                           (Qualified
                                                              { qualifiedPredicates = []
                                                              , qualifiedType =
                                                                  ConstructorType
                                                                    (TypeConstructor
                                                                       { typeConstructorIdentifier =
                                                                           TypeName
                                                                             5
                                                                             "Integer"
                                                                       , typeConstructorKind =
                                                                           StarKind
                                                                       })
                                                              })
                                                     })
                                                  (IntegerLiteral 3))
                                               ( "*"
                                               , ApplicationExpression
                                                   (TypeSignature
                                                      { typeSignatureA =
                                                          Location
                                                            { locationStartLine = 0
                                                            , locationStartColumn = 0
                                                            , locationEndLine = 0
                                                            , locationEndColumn = 0
                                                            }
                                                      , typeSignatureScheme =
                                                          Forall
                                                            []
                                                            (Qualified
                                                               { qualifiedPredicates =
                                                                   [ IsIn
                                                                       (ClassName
                                                                          13
                                                                          "Num")
                                                                       [ ConstructorType
                                                                           (TypeConstructor
                                                                              { typeConstructorIdentifier =
                                                                                  TypeName
                                                                                    5
                                                                                    "Integer"
                                                                              , typeConstructorKind =
                                                                                  StarKind
                                                                              })
                                                                       ]
                                                                   ]
                                                               , qualifiedType =
                                                                   ApplicationType
                                                                     (ApplicationType
                                                                        (ConstructorType
                                                                           (TypeConstructor
                                                                              { typeConstructorIdentifier =
                                                                                  TypeName
                                                                                    2
                                                                                    "(->)"
                                                                              , typeConstructorKind =
                                                                                  FunctionKind
                                                                                    StarKind
                                                                                    (FunctionKind
                                                                                       StarKind
                                                                                       StarKind)
                                                                              }))
                                                                        (ConstructorType
                                                                           (TypeConstructor
                                                                              { typeConstructorIdentifier =
                                                                                  TypeName
                                                                                    5
                                                                                    "Integer"
                                                                              , typeConstructorKind =
                                                                                  StarKind
                                                                              })))
                                                                     (ApplicationType
                                                                        (ApplicationType
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       2
                                                                                       "(->)"
                                                                                 , typeConstructorKind =
                                                                                     FunctionKind
                                                                                       StarKind
                                                                                       (FunctionKind
                                                                                          StarKind
                                                                                          StarKind)
                                                                                 }))
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       5
                                                                                       "Integer"
                                                                                 , typeConstructorKind =
                                                                                     StarKind
                                                                                 })))
                                                                        (ConstructorType
                                                                           (TypeConstructor
                                                                              { typeConstructorIdentifier =
                                                                                  TypeName
                                                                                    5
                                                                                    "Integer"
                                                                              , typeConstructorKind =
                                                                                  StarKind
                                                                              })))
                                                               })
                                                      })
                                                   (VariableExpression
                                                      (TypeSignature
                                                         { typeSignatureA =
                                                             Location
                                                               { locationStartLine = 0
                                                               , locationStartColumn = 0
                                                               , locationEndLine = 0
                                                               , locationEndColumn = 0
                                                               }
                                                         , typeSignatureScheme =
                                                             Forall
                                                               []
                                                               (Qualified
                                                                  { qualifiedPredicates =
                                                                      [ IsIn
                                                                          (ClassName
                                                                             13
                                                                             "Num")
                                                                          [ ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       5
                                                                                       "Integer"
                                                                                 , typeConstructorKind =
                                                                                     StarKind
                                                                                 })
                                                                          ]
                                                                      ]
                                                                  , qualifiedType =
                                                                      ApplicationType
                                                                        (ApplicationType
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       2
                                                                                       "(->)"
                                                                                 , typeConstructorKind =
                                                                                     FunctionKind
                                                                                       StarKind
                                                                                       (FunctionKind
                                                                                          StarKind
                                                                                          StarKind)
                                                                                 }))
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       5
                                                                                       "Integer"
                                                                                 , typeConstructorKind =
                                                                                     StarKind
                                                                                 })))
                                                                        (ApplicationType
                                                                           (ApplicationType
                                                                              (ConstructorType
                                                                                 (TypeConstructor
                                                                                    { typeConstructorIdentifier =
                                                                                        TypeName
                                                                                          2
                                                                                          "(->)"
                                                                                    , typeConstructorKind =
                                                                                        FunctionKind
                                                                                          StarKind
                                                                                          (FunctionKind
                                                                                             StarKind
                                                                                             StarKind)
                                                                                    }))
                                                                              (ConstructorType
                                                                                 (TypeConstructor
                                                                                    { typeConstructorIdentifier =
                                                                                        TypeName
                                                                                          5
                                                                                          "Integer"
                                                                                    , typeConstructorKind =
                                                                                        StarKind
                                                                                    })))
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       5
                                                                                       "Integer"
                                                                                 , typeConstructorKind =
                                                                                     StarKind
                                                                                 })))
                                                                  })
                                                         })
                                                      (MethodName 12 "times"))
                                                   (VariableExpression
                                                      (TypeSignature
                                                         { typeSignatureA =
                                                             Location
                                                               { locationStartLine = 0
                                                               , locationStartColumn = 0
                                                               , locationEndLine = 0
                                                               , locationEndColumn = 0
                                                               }
                                                         , typeSignatureScheme =
                                                             Forall
                                                               []
                                                               (Qualified
                                                                  { qualifiedPredicates =
                                                                      [ IsIn
                                                                          (ClassName
                                                                             13
                                                                             "Num")
                                                                          [ ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       5
                                                                                       "Integer"
                                                                                 , typeConstructorKind =
                                                                                     StarKind
                                                                                 })
                                                                          ]
                                                                      ]
                                                                  , qualifiedType =
                                                                      ApplicationType
                                                                        (ApplicationType
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       2
                                                                                       "(->)"
                                                                                 , typeConstructorKind =
                                                                                     FunctionKind
                                                                                       StarKind
                                                                                       (FunctionKind
                                                                                          StarKind
                                                                                          StarKind)
                                                                                 }))
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       5
                                                                                       "Integer"
                                                                                 , typeConstructorKind =
                                                                                     StarKind
                                                                                 })))
                                                                        (ApplicationType
                                                                           (ApplicationType
                                                                              (ConstructorType
                                                                                 (TypeConstructor
                                                                                    { typeConstructorIdentifier =
                                                                                        TypeName
                                                                                          2
                                                                                          "(->)"
                                                                                    , typeConstructorKind =
                                                                                        FunctionKind
                                                                                          StarKind
                                                                                          (FunctionKind
                                                                                             StarKind
                                                                                             StarKind)
                                                                                    }))
                                                                              (ConstructorType
                                                                                 (TypeConstructor
                                                                                    { typeConstructorIdentifier =
                                                                                        TypeName
                                                                                          5
                                                                                          "Integer"
                                                                                    , typeConstructorKind =
                                                                                        StarKind
                                                                                    })))
                                                                           (ConstructorType
                                                                              (TypeConstructor
                                                                                 { typeConstructorIdentifier =
                                                                                     TypeName
                                                                                       5
                                                                                       "Integer"
                                                                                 , typeConstructorKind =
                                                                                     StarKind
                                                                                 })))
                                                                  })
                                                         })
                                                      (DictName 37 "$dictNum_Integer")))
                                               (LiteralExpression
                                                  (TypeSignature
                                                     { typeSignatureA =
                                                         Location
                                                           { locationStartLine = 1
                                                           , locationStartColumn = 19
                                                           , locationEndLine = 1
                                                           , locationEndColumn = 20
                                                           }
                                                     , typeSignatureScheme =
                                                         Forall
                                                           []
                                                           (Qualified
                                                              { qualifiedPredicates = []
                                                              , qualifiedType =
                                                                  ConstructorType
                                                                    (TypeConstructor
                                                                       { typeConstructorIdentifier =
                                                                           TypeName
                                                                             5
                                                                             "Integer"
                                                                       , typeConstructorKind =
                                                                           StarKind
                                                                       })
                                                              })
                                                     })
                                                  (IntegerLiteral 6)))
                                         ])
                                }
                            ]
                        }
                    ]
                  ]
              }
          ]))
