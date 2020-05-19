{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module RenameSpec where

import Inflex.Instances ()
import Inflex.Lexer
import Inflex.Renamer
import Inflex.Types
import Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (renameText "" "123")
       (Right
          (LiteralExpression
             (IntegerLiteral
                Integery
                  { location =
                      Location
                        { start = SourcePos {name = "", line = 1, column = 1}
                        , end = SourcePos {name = "", line = 1, column = 4}
                        }
                  , integer = 123
                  , typ = ()
                  }))))
  it
    "Lambda"
    (shouldBe
       (renameText "" "\\->123")
       (Right
          (LambdaExpression
             (Lambda
                { typ = ()
                , location =
                    Location
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 7, name = ""}
                      }
                , body =
                    LiteralExpression
                      (IntegerLiteral
                         (Integery
                            { location =
                                Location
                                  { start =
                                      SourcePos
                                        {line = 1, column = 4, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 7, name = ""}
                                  }
                            , integer = 123
                            , typ = ()
                            }))
                }))))
