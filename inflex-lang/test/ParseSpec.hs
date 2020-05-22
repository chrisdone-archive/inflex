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
       (parseText "" "\\x->123")
       (Right
          (LambdaExpression
             (Lambda
                { location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 8, name = ""}
                      }
                , param =
                    Param
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 3, name = ""}
                            }
                      , name = "x"
                      , typ = ()
                      }
                , body =
                    LiteralExpression
                      (IntegerLiteral
                         (Integery
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos
                                        {line = 1, column = 5, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 8, name = ""}
                                  }
                            , integer = 123
                            , typ = ()
                            }))
                , typ = ()
                }))))
