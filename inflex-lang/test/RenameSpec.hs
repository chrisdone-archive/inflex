{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module RenameSpec where

import           Data.Map.Strict (Map)
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
                 (Integery {location = FinalCursor, integer = 123, typ = ()}))
          , M.fromList
              [ ( FinalCursor
                , SourceLocation
                    { start = SourcePos {line = 1, column = 1, name = ""}
                    , end = SourcePos {line = 1, column = 4, name = ""}
                    })
              ])))
  it
    "Lambda"
    (shouldBe
       (renameText "" "\\->123")
       (Right
          ( LambdaExpression
              (Lambda
                 { location = FinalCursor
                 , body =
                     LiteralExpression
                       (IntegerLiteral
                          (Integery
                             { location = InLambdaCursor FinalCursor
                             , integer = 123
                             , typ = ()
                             }))
                 , typ = ()
                 })
          , M.fromList
              [ ( FinalCursor
                , SourceLocation
                    { start = SourcePos {line = 1, column = 1, name = ""}
                    , end = SourcePos {line = 1, column = 7, name = ""}
                    })
              , ( InLambdaCursor FinalCursor
                , SourceLocation
                    { start = SourcePos {line = 1, column = 4, name = ""}
                    , end = SourcePos {line = 1, column = 7, name = ""}
                    })
              ])))
