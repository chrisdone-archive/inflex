{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module SolverSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import           Inflex.Instances ()
import           Inflex.Solver
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (solveText "" "123")
       (Right
          (IsSolved
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
             })))
  it
    "Lambda"
    (shouldBe
       (solveText "" "\\x->123")
       (Right
          (IsSolved
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
