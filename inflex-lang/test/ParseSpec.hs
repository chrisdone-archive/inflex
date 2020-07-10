{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test the parser.

module ParseSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import           Inflex.Instances ()
import           Inflex.Lexer
import           Inflex.Parser
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  types
  literals
  globals
  lambda
  apply
  it
    "Let"
    (shouldBe
       (parseText "" "let x = 1 in x")
       (Right
          (LetExpression
             (Let
                { location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 15, name = ""}
                      }
                , binds =
                    Bind
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 5, name = ""}
                            , end = SourcePos {line = 1, column = 10, name = ""}
                            }
                      , param =
                          Param
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos
                                        {line = 1, column = 5, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 6, name = ""}
                                  }
                            , name = "x"
                            , typ = Nothing
                            }
                      , value =
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
                                  , number = IntegerNumber 1
                                  , typ = Nothing
                                  }))
                      , typ = Nothing
                      } :|
                    []
                , body =
                    VariableExpression
                      (Variable
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 14, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 15, name = ""}
                               }
                         , name = "x"
                         , typ = Nothing
                         })
                , typ = Nothing
                }))))
  it
    "Let many defs"
    (shouldBe
       (parseText "" "let x = 1; y = 2 in x")
       (Right
          (LetExpression
             (Let
                { location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 22, name = ""}
                      }
                , binds =
                    Bind
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 5, name = ""}
                            , end = SourcePos {line = 1, column = 10, name = ""}
                            }
                      , param =
                          Param
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos
                                        {line = 1, column = 5, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 6, name = ""}
                                  }
                            , name = "x"
                            , typ = Nothing
                            }
                      , value =
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
                                  , number = IntegerNumber 1
                                  , typ = Nothing
                                  }))
                      , typ = Nothing
                      } :|
                    [ Bind
                        { location =
                            SourceLocation
                              { start =
                                  SourcePos {line = 1, column = 12, name = ""}
                              , end =
                                  SourcePos {line = 1, column = 17, name = ""}
                              }
                        , param =
                            Param
                              { location =
                                  SourceLocation
                                    { start =
                                        SourcePos
                                          {line = 1, column = 12, name = ""}
                                    , end =
                                        SourcePos
                                          {line = 1, column = 13, name = ""}
                                    }
                              , name = "y"
                              , typ = Nothing
                              }
                        , value =
                            LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location =
                                        SourceLocation
                                          { start =
                                              SourcePos
                                                { line = 1
                                                , column = 16
                                                , name = ""
                                                }
                                          , end =
                                              SourcePos
                                                { line = 1
                                                , column = 17
                                                , name = ""
                                                }
                                          }
                                    , number = IntegerNumber 2
                                    , typ = Nothing
                                    }))
                        , typ = Nothing
                        }
                    ]
                , body =
                    VariableExpression
                      (Variable
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 21, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 22, name = ""}
                               }
                         , name = "x"
                         , typ = Nothing
                         })
                , typ = Nothing
                }))))
  describe "Operators" ops


ops :: Spec
ops = do
  it
    "x+y"
    (shouldBe
       (parseText "" "x+y")
       (Right
          (InfixExpression
             (Infix
                { location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 4, name = ""}
                      }
                , global =
                    Global
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
                            , end = SourcePos {line = 1, column = 4, name = ""}
                            }
                      , name = "+"
                      , scheme = ParsedScheme
                      }
                , left =
                    VariableExpression
                      (Variable
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 2, name = ""}
                               }
                         , name = "x"
                         , typ = Nothing
                         })
                , right =
                    VariableExpression
                      (Variable
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 3, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 4, name = ""}
                               }
                         , name = "y"
                         , typ = Nothing
                         })
                , typ = Nothing
                }))))
  it
    "x+y-z*y/2"
    (shouldBe
       (parseText "" "x+y-z*y/2")
       (Right
          (InfixExpression
             (Infix
                { location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 10, name = ""}
                      }
                , global =
                    Global
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
                            , end = SourcePos {line = 1, column = 10, name = ""}
                            }
                      , name = "-"
                      , scheme = ParsedScheme
                      }
                , left =
                    InfixExpression
                      (Infix
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 4, name = ""}
                               }
                         , global =
                             Global
                               { location =
                                   SourceLocation
                                     { start =
                                         SourcePos
                                           {line = 1, column = 1, name = ""}
                                     , end =
                                         SourcePos
                                           {line = 1, column = 4, name = ""}
                                     }
                               , name = "+"
                               , scheme = ParsedScheme
                               }
                         , left =
                             VariableExpression
                               (Variable
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 1, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 2, name = ""}
                                        }
                                  , name = "x"
                                  , typ = Nothing
                                  })
                         , right =
                             VariableExpression
                               (Variable
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 3, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 4, name = ""}
                                        }
                                  , name = "y"
                                  , typ = Nothing
                                  })
                         , typ = Nothing
                         })
                , right =
                    InfixExpression
                      (Infix
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 5, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 10, name = ""}
                               }
                         , global =
                             Global
                               { location =
                                   SourceLocation
                                     { start =
                                         SourcePos
                                           {line = 1, column = 5, name = ""}
                                     , end =
                                         SourcePos
                                           {line = 1, column = 10, name = ""}
                                     }
                               , name = "*"
                               , scheme = ParsedScheme
                               }
                         , left =
                             VariableExpression
                               (Variable
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 5, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 6, name = ""}
                                        }
                                  , name = "z"
                                  , typ = Nothing
                                  })
                         , right =
                             InfixExpression
                               (Infix
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 7, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 10, name = ""}
                                        }
                                  , global =
                                      Global
                                        { location =
                                            SourceLocation
                                              { start =
                                                  SourcePos
                                                    { line = 1
                                                    , column = 7
                                                    , name = ""
                                                    }
                                              , end =
                                                  SourcePos
                                                    { line = 1
                                                    , column = 10
                                                    , name = ""
                                                    }
                                              }
                                        , name = "/"
                                        , scheme = ParsedScheme
                                        }
                                  , left =
                                      VariableExpression
                                        (Variable
                                           { location =
                                               SourceLocation
                                                 { start =
                                                     SourcePos
                                                       { line = 1
                                                       , column = 7
                                                       , name = ""
                                                       }
                                                 , end =
                                                     SourcePos
                                                       { line = 1
                                                       , column = 8
                                                       , name = ""
                                                       }
                                                 }
                                           , name = "y"
                                           , typ = Nothing
                                           })
                                  , right =
                                      LiteralExpression
                                        (NumberLiteral
                                           (Number
                                              { location =
                                                  SourceLocation
                                                    { start =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 9
                                                          , name = ""
                                                          }
                                                    , end =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 10
                                                          , name = ""
                                                          }
                                                    }
                                              , number = IntegerNumber 2
                                              , typ = Nothing
                                              }))
                                  , typ = Nothing
                                  })
                         , typ = Nothing
                         })
                , typ = Nothing
                }))))
  it
    "x+y+z"
    (shouldBe
       (parseText "" "x+y+z")
       (Right
          (InfixExpression
             (Infix
                { location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 6, name = ""}
                      }
                , global =
                    Global
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
                            , end = SourcePos {line = 1, column = 6, name = ""}
                            }
                      , name = "+"
                      , scheme = ParsedScheme
                      }
                , left =
                    InfixExpression
                      (Infix
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 4, name = ""}
                               }
                         , global =
                             Global
                               { location =
                                   SourceLocation
                                     { start =
                                         SourcePos
                                           {line = 1, column = 1, name = ""}
                                     , end =
                                         SourcePos
                                           {line = 1, column = 4, name = ""}
                                     }
                               , name = "+"
                               , scheme = ParsedScheme
                               }
                         , left =
                             VariableExpression
                               (Variable
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 1, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 2, name = ""}
                                        }
                                  , name = "x"
                                  , typ = Nothing
                                  })
                         , right =
                             VariableExpression
                               (Variable
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 3, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 4, name = ""}
                                        }
                                  , name = "y"
                                  , typ = Nothing
                                  })
                         , typ = Nothing
                         })
                , right =
                    VariableExpression
                      (Variable
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 5, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 6, name = ""}
                               }
                         , name = "z"
                         , typ = Nothing
                         })
                , typ = Nothing
                }))))
  it
    "x+ [should error]"
    (shouldBe
       (parseText "" "x+")
       (Left
          (ParseError
             (ParseErrors
                (NoMoreInput :|
                 [ NoMoreInput
                 , NoMoreInput
                 , NoMoreInput
                 , NoMoreInput
                 , NoMoreInput
                 , NoMoreInput
                 , NoMoreInput
                 ])))))
  it
    "x + + y [should error]"
    (shouldBe
       (parseText "" "x + + x")
       (Left
          (ParseError
             (ParseErrors
                (ExpectedLet :|
                 [ ExpectedVariable
                 , ExpectedToken OpenRoundToken
                 , ExpectedInteger
                 , ExpectedDecimal
                 , ExpectedToken BackslashToken
                 , ExpectedVariable
                 , ExpectedToken OpenRoundToken
                 ])))))

types :: Spec
types = describe
          "Type"
          (do it
                "Integer"
                (shouldBe
                   (parseType "" "Integer")
                   (Right
                      (ConstantType
                         (TypeConstant
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos {line = 1, column = 1, name = ""}
                                  , end = SourcePos {line = 1, column = 8, name = ""}
                                  }
                            , name = IntegerTypeName
                            }))))
              it
                "Decimal 3"
                (shouldBe
                   (parseType "" "Decimal 3")
                   (Right
                      (ApplyType
                         (TypeApplication
                            { function =
                                ConstantType
                                  (TypeConstant
                                     { location =
                                         SourceLocation
                                           { start =
                                               SourcePos
                                                 {line = 1, column = 1, name = ""}
                                           , end =
                                               SourcePos
                                                 {line = 1, column = 8, name = ""}
                                           }
                                     , name = DecimalTypeName
                                     })
                            , argument =
                                ConstantType
                                  (TypeConstant
                                     { location =
                                         SourceLocation
                                           { start =
                                               SourcePos
                                                 {line = 1, column = 9, name = ""}
                                           , end =
                                               SourcePos
                                                 {line = 1, column = 10, name = ""}
                                           }
                                     , name = NatTypeName 3
                                     })
                            , location =
                                SourceLocation
                                  { start =
                                      SourcePos {line = 1, column = 1, name = ""}
                                  , end = SourcePos {line = 1, column = 10, name = ""}
                                  }
                            , kind = TypeKind
                            }))))
              it
                "Integer -> Decimal 3"
                (shouldBe
                   (parseType "" "Integer->Decimal 3")
                   (Right
                      (ApplyType
                         (TypeApplication
                            { function =
                                ApplyType
                                  (TypeApplication
                                     { function =
                                         ConstantType
                                           (TypeConstant
                                              { location =
                                                  SourceLocation
                                                    { start =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 8
                                                          , name = ""
                                                          }
                                                    , end =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 10
                                                          , name = ""
                                                          }
                                                    }
                                              , name = FunctionTypeName
                                              })
                                     , argument =
                                         ConstantType
                                           (TypeConstant
                                              { location =
                                                  SourceLocation
                                                    { start =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 1
                                                          , name = ""
                                                          }
                                                    , end =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 8
                                                          , name = ""
                                                          }
                                                    }
                                              , name = IntegerTypeName
                                              })
                                     , location =
                                         SourceLocation
                                           { start =
                                               SourcePos
                                                 {line = 1, column = 8, name = ""}
                                           , end =
                                               SourcePos
                                                 {line = 1, column = 10, name = ""}
                                           }
                                     , kind = FunKind TypeKind TypeKind
                                     })
                            , argument =
                                ApplyType
                                  (TypeApplication
                                     { function =
                                         ConstantType
                                           (TypeConstant
                                              { location =
                                                  SourceLocation
                                                    { start =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 10
                                                          , name = ""
                                                          }
                                                    , end =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 17
                                                          , name = ""
                                                          }
                                                    }
                                              , name = DecimalTypeName
                                              })
                                     , argument =
                                         ConstantType
                                           (TypeConstant
                                              { location =
                                                  SourceLocation
                                                    { start =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 18
                                                          , name = ""
                                                          }
                                                    , end =
                                                        SourcePos
                                                          { line = 1
                                                          , column = 19
                                                          , name = ""
                                                          }
                                                    }
                                              , name = NatTypeName 3
                                              })
                                     , location =
                                         SourceLocation
                                           { start =
                                               SourcePos
                                                 {line = 1, column = 10, name = ""}
                                           , end =
                                               SourcePos
                                                 {line = 1, column = 19, name = ""}
                                           }
                                     , kind = TypeKind
                                     })
                            , location =
                                SourceLocation
                                  { start =
                                      SourcePos {line = 1, column = 8, name = ""}
                                  , end = SourcePos {line = 1, column = 10, name = ""}
                                  }
                            , kind = TypeKind
                            })))))

literals :: Spec
literals = it
             "Literal"
             (do shouldBe
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
                              , number = IntegerNumber 123
                              , typ = Nothing
                              })))
                 shouldBe
                   (parseText "" "123.0")
                   (Right
                      (LiteralExpression
                         (NumberLiteral
                            (Number
                               { location =
                                   SourceLocation
                                     { start =
                                         SourcePos {line = 1, column = 1, name = ""}
                                     , end = SourcePos {line = 1, column = 6, name = ""}
                                     }
                               , number =
                                   DecimalNumber (Decimal {places = 1, integer = 1230})
                               , typ = Nothing
                               }))))
                 shouldBe
                   (parseText "" "123.123")
                   (Right
                      (LiteralExpression
                         (NumberLiteral
                            (Number
                               { location =
                                   SourceLocation
                                     { start =
                                         SourcePos {line = 1, column = 1, name = ""}
                                     , end = SourcePos {line = 1, column = 8, name = ""}
                                     }
                               , number =
                                   DecimalNumber (Decimal {places = 3, integer = 123123})
                               , typ = Nothing
                               }))))
                 shouldBe
                   (parseText "" "0.000")
                   (Right
                      (LiteralExpression
                         (NumberLiteral
                            (Number
                               { location =
                                   SourceLocation
                                     { start =
                                         SourcePos {line = 1, column = 1, name = ""}
                                     , end = SourcePos {line = 1, column = 6, name = ""}
                                     }
                               , number =
                                   DecimalNumber (Decimal {places = 3, integer = 0})
                               , typ = Nothing
                               })))))

globals :: Spec
globals =
  it
    "Globals"
    (do shouldBe
          (parseText "" "abc")
          (Right
             (VariableExpression
                (Variable
                   { location =
                       SourceLocation
                         { start = SourcePos {line = 1, column = 1, name = ""}
                         , end = SourcePos {line = 1, column = 4, name = ""}
                         }
                   , name = "abc"
                   , typ = Nothing
                   })))
        shouldBe
          (parseText "" "\\x->y")
          (Right
             (LambdaExpression
                (Lambda
                   { location =
                       SourceLocation
                         { start = SourcePos {line = 1, column = 1, name = ""}
                         , end = SourcePos {line = 1, column = 6, name = ""}
                         }
                   , param =
                       Param
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 2, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 3, name = ""}
                               }
                         , name = "x"
                         , typ = Nothing
                         }
                   , body =
                       VariableExpression
                         (Variable
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos
                                        {line = 1, column = 5, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 6, name = ""}
                                  }
                            , name = "y"
                            , typ = Nothing
                            })
                   , typ = Nothing
                   })))
        shouldBe
          (parseText "" "\\x->x y")
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
                               , end =
                                   SourcePos {line = 1, column = 3, name = ""}
                               }
                         , name = "x"
                         , typ = Nothing
                         }
                   , body =
                       ApplyExpression
                         (Apply
                            { location =
                                SourceLocation
                                  { start =
                                      SourcePos
                                        {line = 1, column = 5, name = ""}
                                  , end =
                                      SourcePos
                                        {line = 1, column = 8, name = ""}
                                  }
                            , function =
                                VariableExpression
                                  (Variable
                                     { location =
                                         SourceLocation
                                           { start =
                                               SourcePos
                                                 { line = 1
                                                 , column = 5
                                                 , name = ""
                                                 }
                                           , end =
                                               SourcePos
                                                 { line = 1
                                                 , column = 6
                                                 , name = ""
                                                 }
                                           }
                                     , name = "x"
                                     , typ = Nothing
                                     })
                            , argument =
                                VariableExpression
                                  (Variable
                                     { location =
                                         SourceLocation
                                           { start =
                                               SourcePos
                                                 { line = 1
                                                 , column = 7
                                                 , name = ""
                                                 }
                                           , end =
                                               SourcePos
                                                 { line = 1
                                                 , column = 8
                                                 , name = ""
                                                 }
                                           }
                                     , name = "y"
                                     , typ = Nothing
                                     })
                            , typ = Nothing
                            })
                   , typ = Nothing
                   }))))

lambda :: Spec
lambda = it
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
                             , typ = Nothing
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
                                   , number = IntegerNumber 123
                                   , typ = Nothing
                                   }))
                       , typ = Nothing
                       }))))

apply :: SpecWith ()
apply = it
          "Apply"
          (shouldBe
             (parseText "" "(\\x->x) 1")
             (Right
                (ApplyExpression
                   (Apply
                      { location =
                          SourceLocation
                            { start = SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 10, name = ""}
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
                                     , typ = Nothing
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
                                        , typ = Nothing
                                        })
                               , typ = Nothing
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
                                  , number = IntegerNumber 1
                                  , typ = Nothing
                                  }))
                      , typ = Nothing
                      }))))
