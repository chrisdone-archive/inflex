{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

import "monad-logger" Control.Monad.Logger
import "duet"         Control.Monad.Supply
import                Control.Monad.Writer
import                Data.Bifunctor
import                Data.Maybe
import                Data.Text (Text)
import qualified      Data.Text as T
import                Debug.Trace
import                Duet.Infer
import                Duet.Parser
import                Duet.Printer
import                Duet.Simple
import                Duet.Types
import qualified      Language.PureScript.CST as CST
import                Test.Hspec
import                Test.Validity (forAllUnchecked)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec =
  describe
    "Compilation"
    (do describe
          "Property"
          (do it
                "Compiler terminates"
                (forAllUnchecked
                   (\decls ->
                      shouldBe
                        (either
                           (const ())
                           (const ())
                           (runNoLoggingT
                              (evalSupplyT
                                 (do (_binds, _ctx) <- createContext decls
                                     pure ())
                                 [1 ..])))
                        ()))
              it
                "PureScript"
                (forAllUnchecked
                   (\(decls :: [( Location
                                , Binding UnkindedType Identifier Location)]) ->
                      shouldBe
                        (either
                           (const ())
                           (const ())
                           (runNoLoggingT
                              (evalSupplyT
                                 (do (binds, ctx) <-
                                       createContext
                                         (fmap (uncurry BindDecl) decls)
                                     let inputSource =
                                           T.pack
                                             ("module X where\n" <>
                                              concatMap
                                                (\b -> printBinding
                                                         defaultPrint
                                                         (contextSpecialTypes ctx) b <> "\n")
                                                (concatMap
                                                   (\(BindGroup explicit implicit) ->
                                                      map
                                                        ExplicitBinding
                                                        explicit <>
                                                      concatMap
                                                        (map ImplicitBinding)
                                                        implicit)
                                                   binds))
                                     trace
                                       ("\n\n" ++ T.unpack inputSource) (pure ())
                                     case CST.parseFromFile
                                            "DuetGenerated"
                                            inputSource of
                                       Left e -> do
                                         pure (Left ())
                                       Right {} -> pure (Right ()))
                                 [1 ..])))
                        ())))
        it
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
                          [LiteralExpression () (IntegerLiteral 1)]
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
                          [ InfixExpression
                              ()
                              (LiteralExpression () (IntegerLiteral 2))
                              ( "*"
                              , ApplicationExpression
                                  ()
                                  (VariableExpression () (MethodName 12 "times"))
                                  (VariableExpression
                                     ()
                                     (DictName 37 "$dictNum_Integer")))
                              (LiteralExpression () (IntegerLiteral 3))
                          , LiteralExpression () (IntegerLiteral 5)
                          ]
                      , ArrayExpression
                          ()
                          [ LiteralExpression () (IntegerLiteral 6)
                          , LiteralExpression () (IntegerLiteral 5)
                          ]
                      ]))))
