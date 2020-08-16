{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

-- | Document loading spec.

module DocumentSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
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
    (do u1 <- nextRandom
        shouldBe
          (let loaded =
                 loadDocument [Named {uuid = Uuid u1, name = "x", thing = "x"}]
            in evalDocument (evalEnvironment loaded) (defaultDocument loaded))
          (Toposorted
             { unToposorted =
                 [ Named
                     { uuid = (Uuid u1)
                     , name = "x"
                     , thing = Left (CycleError ["x"])
                     }
                 ]
             }))
  it
    "x = y"
    (do u1 <- nextRandom
        shouldBe
          (let loaded =
                 loadDocument [Named {uuid = Uuid u1, name = "x", thing = "y"}]
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
                     }
                 ]
             }))
  it
    "y = 1; x = y y"
    (do u1 <- nextRandom
        u2 <- nextRandom
        shouldBe
          (let loaded =
                 loadDocument
                   [ Named {uuid = Uuid u1, name = "x", thing = "y y"}
                   , Named {uuid = Uuid u2, name = "y", thing = "1"}
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
                     }
                 ]
             }))

success :: SpecWith ()
success = do
  it
    "x = y + 2; z = 2; y = z * 3.1"
    (do u1 <- nextRandom
        u2 <- nextRandom
        u3 <- nextRandom
        shouldBe
          (let loaded =
                 loadDocument
                   [ Named {uuid = Uuid u1, name = "x", thing = "y + 2"}
                   , Named {uuid = Uuid u2, name = "y", thing = "z * 3.1"}
                   , Named {uuid = Uuid u3, name = "z", thing = "2"}
                   ]
            in evalDocument (evalEnvironment loaded) (defaultDocument loaded))
          (Toposorted
             { unToposorted =
                 [ Named
                     { uuid = (Uuid u3)
                     , name = "z"
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
                     { uuid = (Uuid u2)
                     , name = "y"
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
                     { uuid = (Uuid u1)
                     , name = "x"
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
