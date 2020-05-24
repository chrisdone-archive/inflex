{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module SolverSpec where

import qualified Data.Map.Strict as M
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
          IsSolved
            { thing =
                LiteralExpression
                  (IntegerLiteral
                     (Integery
                        {location = ExpressionCursor, integer = 123, typ = ()}))
            , mappings =
                M.fromList
                  [ ( ExpressionCursor
                    , SourceLocation
                        { start = SourcePos {line = 1, column = 1, name = ""}
                        , end = SourcePos {line = 1, column = 4, name = ""}
                        })
                  ]
            }))
  it
    "Lambda"
    (shouldBe
       (solveText "" "\\x->123")
       (Right
          IsSolved
            { thing =
                LambdaExpression
                  (Lambda
                     { location = ExpressionCursor
                     , param =
                         Param
                           {location = LambdaParamCursor, name = (), typ = ()}
                     , body =
                         LiteralExpression
                           (IntegerLiteral
                              (Integery
                                 { location = LambdaBodyCursor ExpressionCursor
                                 , integer = 123
                                 , typ = ()
                                 }))
                     , typ = ()
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
            }))