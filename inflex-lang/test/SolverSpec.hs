{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module SolverSpec where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Inflex.Instances ()
import           Inflex.Solver
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do describe "Fine-grained" fineGrained
          describe "Coarse-grained" coarseGrained

--------------------------------------------------------------------------------
-- Coarse-grained tests

coarseGrained :: Spec
coarseGrained =
  describe
    "Successful"
    (do it
          "123"
          (shouldBe
             (solveText "" "123")
             (Right
                (IsSolved
                   { thing =
                       LiteralExpression
                         (IntegerLiteral
                            (Integery
                               { location = ExpressionCursor
                               , integer = 123
                               , typ =
                                   VariableType
                                     (TypeVariable
                                        { location = ExpressionCursor
                                        , prefix = IntegeryPrefix
                                        , index = 0
                                        })
                               }))
                   , mappings =
                       M.fromList
                         [ ( ExpressionCursor
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 4, name = ""}
                               })
                         ]
                   , classes =
                       Seq.fromList
                         [ ClassConstraint
                             { className = FromIntegerClassName
                             , types =
                                 pure
                                   (VariableType
                                      (TypeVariable
                                         { location = ExpressionCursor
                                         , prefix = IntegeryPrefix
                                         , index = 0
                                         }))
                             , location = ExpressionCursor
                             }
                         ]
                   })))
        it
          "(\\x->x)123"
          (shouldBe
             (solveText "" "(\\x->x)123")
             (Right
                (IsSolved
                   { thing =
                       ApplyExpression
                         (Apply
                            { location = ExpressionCursor
                            , function =
                                LambdaExpression
                                  (Lambda
                                     { location =
                                         ApplyFuncCursor ExpressionCursor
                                     , param =
                                         Param
                                           { location =
                                               ApplyFuncCursor LambdaParamCursor
                                           , name = ()
                                           , typ =
                                               VariableType
                                                 (TypeVariable
                                                    { location =
                                                        ApplyArgCursor
                                                          ExpressionCursor
                                                    , prefix = ApplyPrefix
                                                    , index = 3
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
                                                           ApplyArgCursor
                                                             ExpressionCursor
                                                       , prefix = ApplyPrefix
                                                       , index = 3
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
                                                                    ApplyArgCursor
                                                                      ExpressionCursor
                                                                , prefix =
                                                                    ApplyPrefix
                                                                , index = 3
                                                                })
                                                       , location =
                                                           ApplyFuncCursor
                                                             ExpressionCursor
                                                       })
                                              , argument =
                                                  VariableType
                                                    (TypeVariable
                                                       { location =
                                                           ApplyArgCursor
                                                             ExpressionCursor
                                                       , prefix = ApplyPrefix
                                                       , index = 3
                                                       })
                                              , location =
                                                  ApplyFuncCursor
                                                    ExpressionCursor
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
                                                 , prefix = ApplyPrefix
                                                 , index = 3
                                                 })
                                        }))
                            , typ =
                                VariableType
                                  (TypeVariable
                                     { location =
                                         ApplyArgCursor ExpressionCursor
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
                               , end =
                                   SourcePos {line = 1, column = 7, name = ""}
                               })
                         , ( ApplyFuncCursor ExpressionCursor
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 2, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 7, name = ""}
                               })
                         , ( ApplyFuncCursor (LambdaBodyCursor ExpressionCursor)
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 6, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 7, name = ""}
                               })
                         , ( ApplyFuncCursor LambdaParamCursor
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 3, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 4, name = ""}
                               })
                         , ( ApplyArgCursor ExpressionCursor
                           , SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 8, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 11, name = ""}
                               })
                         ]
                   , classes =
                       [ ClassConstraint
                           { className = FromIntegerClassName
                           , types =
                               pure
                                 (VariableType
                                    (TypeVariable
                                       { location =
                                           ApplyArgCursor ExpressionCursor
                                       , prefix = ApplyPrefix
                                       , index = 3
                                       }))
                           , location = ApplyArgCursor ExpressionCursor
                           }
                       ]
                   }))))


--------------------------------------------------------------------------------
-- Fine-grained tests

fineGrained :: Spec
fineGrained = do
  describe
    "Successful"
    (do it "a ~ a" (shouldBe (unifyConstraints [a .~ a]) (pure []))
        it
          "Integer ~ Integer"
          (shouldBe (unifyConstraints [_Integer .~ _Integer]) (pure []))
        it "a ~ b" (shouldBe (unifyConstraints [a .~ b]) (pure [a' .+-> b]))
        it
          "a ~ Integer"
          (shouldBe (unifyConstraints [a .~ _Integer]) (pure [a' .+-> _Integer]))
        it
          "F a Text ~ F Integer b"
          (shouldBe
             (unifyConstraints [_F a _Text .~ _F _Integer b])
             (pure [a' .+-> _Integer, b' .+-> _Text]))
        it
          "F a a ~ F (Option b) (Option Integer)"
          (shouldBe
             (unifyConstraints [_F a a .~ _F (_Option b) (_Option _Integer)])
             (pure [a' .+-> _Option _Integer, b' .+-> _Integer]))
        it
          "(t ~ F a a, F a a ~ F (Option b) (Option Integer)) => t"
          (shouldBe
             (unifyAndSubstitute
                [t .~ _F a a, _F a a .~ _F (_Option b) (_Option _Integer)]
                t)
             (pure (solveType mempty (_F (_Option _Integer) (_Option _Integer))))))
  describe
    "Failing"
    (do it
          "F a ~ a"
          (shouldBe
             (unifyConstraints [_F a .~ a])
             (Left (pure (TypeMismatch (_Integer .~ _Text)))))
        it
          "Integer ~ Text"
          (shouldBe
             (unifyConstraints [_Integer .~ _Text])
             (Left (pure (TypeMismatch (_Integer .~ _Text)))))
        it
          "F a a ~ F (Option Text) (Option Integer)"
          (shouldBe
             (unifyConstraints [_F a a .~ _F (_Option _Text) (_Option _Integer)])
             (Left (pure (TypeMismatch (_Text .~ _Integer))))))

--------------------------------------------------------------------------------
-- Type variables

a' :: TypeVariable Generated
a' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegeryPrefix, index = 0}

b' :: TypeVariable Generated
b' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegeryPrefix, index = 1}

c' :: TypeVariable Generated
c' =
  TypeVariable
    {location = ExpressionCursor, prefix = IntegeryPrefix, index = 2}

--------------------------------------------------------------------------------
-- Types of the variables

t :: Type Generated
t =
  VariableType
    TypeVariable
      {location = ExpressionCursor, prefix = IntegeryPrefix, index = 3}

a :: Type Generated
a = VariableType a'

b :: Type Generated
b = VariableType b'

c :: Type Generated
c = VariableType c'

--------------------------------------------------------------------------------
-- Type constructors

_Integer :: Type Generated
_Integer =
  ConstantType
    TypeConstant {location = ExpressionCursor, name = IntegerTypeName}

_Text :: Type Generated
_Text =
  ConstantType
    TypeConstant {location = ExpressionCursor, name = TextTypeName}

_F :: Type Generated -> Type Generated -> Type Generated
_F x1 x2 =
  ApplyType
    TypeApplication
      { location = ExpressionCursor
      , function =
          ApplyType
            TypeApplication
              { location = ExpressionCursor
              , function =
                  ConstantType
                    TypeConstant
                      {location = ExpressionCursor, name = FunctionTypeName}
              , argument = x1
              }
      , argument = x2
      }

_Option :: Type Generated -> Type Generated
_Option x1 =
  ApplyType
    TypeApplication
      { location = ExpressionCursor
      , function =
          ConstantType
            TypeConstant
              {location = ExpressionCursor, name = OptionTypeName}
      , argument = x1
      }

--------------------------------------------------------------------------------
-- Operators for easier reading

(.~) :: Type Generated -> Type Generated -> EqualityConstraint
(.~) x y =
  EqualityConstraint {location = ExpressionCursor, type1 = x, type2 = y}

(.+->) :: TypeVariable Generated -> Type Generated -> Substitution
(.+->) x y = Substitution {before = x, after = y}
