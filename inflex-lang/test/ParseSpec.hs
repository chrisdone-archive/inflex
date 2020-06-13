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
             (NumberLiteral
                Number
                  { location =
                      SourceLocation
                        { start = SourcePos {name = "", line = 1, column = 1}
                        , end = SourcePos {name = "", line = 1, column = 4}
                        }
                  , number = NaturalNumber 123
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
                      (NumberLiteral
                         (Number
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos
                                        {line = 1, column = 5, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 8, name = ""}
                                  }
                            , number = NaturalNumber 123
                            , typ = ()
                            }))
                , typ = ()
                }))))
  it
    "Apply"
    (shouldBe
       (parseText "" "(\\x->x) 1")
       (Right
          (ApplyExpression
             (Apply
                { location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 2, name = ""}
                      , end = SourcePos {line = 1, column = 7, name = ""}
                      }
                , function =
                    LambdaExpression
                      (Lambda
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 2, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 7, name = ""}
                               }
                         , param =
                             Param
                               { location =
                                   SourceLocation
                                     { start =
                                         SourcePos
                                           {line = 1, column = 3, name = ""}
                                     , end =
                                         SourcePos
                                           {line = 1, column = 4, name = ""}
                                     }
                               , name = "x"
                               , typ = ()
                               }
                         , body =
                             VariableExpression
                               (Variable
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 6, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 7, name = ""}
                                        }
                                  , name = "x"
                                  , typ = ()
                                  })
                         , typ = ()
                         })
                , argument =
                    LiteralExpression
                      (NumberLiteral
                         (Number
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos
                                        {line = 1, column = 9, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 10, name = ""}
                                  }
                            , number = NaturalNumber 1
                            , typ = ()
                            }))
                , typ = ()
                }))))
