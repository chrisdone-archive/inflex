{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module RenameSpec where

import qualified Data.Map.Strict as M
import           Inflex.Instances ()
import           Inflex.Renamer
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (renameText "" "123")
       (Right
          ( LiteralExpression
              (IntegerLiteral
                 (Integery
                    {location = ExpressionCursor, integer = 123, typ = ()}))
          , M.fromList
              [ ( ExpressionCursor
                , SourceLocation
                    { start = SourcePos {line = 1, column = 1, name = ""}
                    , end = SourcePos {line = 1, column = 4, name = ""}
                    })
              ])))
  it
    "Lambda"
    (shouldBe
       (renameText "" "\\x->123")
       (Right
          ( LambdaExpression
              (Lambda
                 { location = ExpressionCursor
                 , param =
                     Param {location = LambdaParamCursor, name = (), typ = ()}
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
          , M.fromList
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
              ])))
