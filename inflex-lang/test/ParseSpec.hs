{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test the parser.

module ParseSpec where

import Inflex.Instances ()
import Inflex.Lexer
import Inflex.Parser
import Inflex.Types
import Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (parseText "" "123")
       (Right
          (LiteralExpression
             (IntegerLiteral
                Integery
                  { location =
                      SourceLocation
                        { start = SourcePos {name = "", line = 1, column = 1}
                        , end = SourcePos {name = "", line = 1, column = 4}
                        }
                  , integer = 123
                  , typ = ()
                  }))))
  it
    "Lambda"
    (shouldBe
       (parseText "" "\\->123")
       (Right
          (LambdaExpression
             (Lambda
                { typ = ()
                , location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 7, name = ""}
                      }
                , body =
                    LiteralExpression
                      (IntegerLiteral
                         (Integery
                            { location =
                                SourceLocation
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
