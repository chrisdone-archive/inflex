{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test the parser.

module ParseSpec where

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector as V
import           Inflex.Instances ()
import           Inflex.Lexer
import           Inflex.Parser
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  sigs
  types
  literals
  globals
  lambda
  apply
  records
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
       (first (const ()) (parseText "" "x+"))
       (Left ()))
  it
    "x + + y [should error]"
    (shouldBe
       (first (const ()) (parseText "" "x + + x"))
       (Left ()))

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
literals =
  it
    "Literal"
    (do shouldBe
          (parseText "" "[]")
          (Right
             (ArrayExpression
                (Array
                   { expressions = V.fromList []
                   , typ = Nothing
                   , location =
                       SourceLocation
                         { start = SourcePos {line = 1, column = 1, name = ""}
                         , end = SourcePos {line = 1, column = 3, name = ""}
                         }
                   })))
        shouldBe
          (parseText "" "[123,123]")
          (Right
             (ArrayExpression
                (Array
                   { expressions =
                       V.fromList
                         [ LiteralExpression
                             (NumberLiteral
                                (Number
                                   { location =
                                       SourceLocation
                                         { start =
                                             SourcePos
                                               {line = 1, column = 2, name = ""}
                                         , end =
                                             SourcePos
                                               {line = 1, column = 5, name = ""}
                                         }
                                   , number = IntegerNumber 123
                                   , typ = Nothing
                                   }))
                         , LiteralExpression
                             (NumberLiteral
                                (Number
                                   { location =
                                       SourceLocation
                                         { start =
                                             SourcePos
                                               {line = 1, column = 6, name = ""}
                                         , end =
                                             SourcePos
                                               {line = 1, column = 9, name = ""}
                                         }
                                   , number = IntegerNumber 123
                                   , typ = Nothing
                                   }))
                         ]
                   , typ = Nothing
                   , location =
                       SourceLocation
                         { start = SourcePos {line = 1, column = 1, name = ""}
                         , end = SourcePos {line = 1, column = 10, name = ""}
                         }
                   })))
        shouldBe
          (parseText "" "-123")
          (Right
             (LiteralExpression
                (NumberLiteral
                   (Number
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 5, name = ""}
                            }
                      , number = IntegerNumber (-123)
                      , typ = Nothing
                      }))))
        shouldBe
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
          (parseText "" "-123.0")
          (Right
             (LiteralExpression
                (NumberLiteral
                   (Number
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 2, name = ""}
                            , end = SourcePos {line = 1, column = 7, name = ""}
                            }
                      , number =
                          DecimalNumber (Decimal {places = 1, integer = -1230})
                      , typ = Nothing
                      }))))
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
          (parseText "" "x:y")
          (Right (LambdaExpression (Lambda {location = SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 4, name = ""}}, param = Param {location = SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 2, name = ""}}, name = "x", typ = Nothing}, body = VariableExpression (Variable {location = SourceLocation {start = SourcePos {line = 1, column = 3, name = ""}, end = SourcePos {line = 1, column = 4, name = ""}}, name = "y", typ = Nothing}), typ = Nothing})))
        shouldBe
          (parseText "" "x:x(y)")
          (Right (LambdaExpression (Lambda {location = SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}}, param = Param {location = SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 2, name = ""}}, name = "x", typ = Nothing}, body = ApplyExpression (Apply {location = SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}}, function = VariableExpression (Variable {location = SourceLocation {start = SourcePos {line = 1, column = 3, name = ""}, end = SourcePos {line = 1, column = 4, name = ""}}, name = "x", typ = Nothing}), argument = VariableExpression (Variable {location = SourceLocation {start = SourcePos {line = 1, column = 5, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}}, name = "y", typ = Nothing}), typ = Nothing}), typ = Nothing}))))

lambda :: Spec
lambda = it
           "Lambda"
           (shouldBe
              (parseText "" "x:123")
              (Right (LambdaExpression (Lambda {location = SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}}, param = Param {location = SourceLocation {start = SourcePos {line = 1, column = 1, name = ""}, end = SourcePos {line = 1, column = 2, name = ""}}, name = "x", typ = Nothing}, body = LiteralExpression (NumberLiteral (Number {location = SourceLocation {start = SourcePos {line = 1, column = 3, name = ""}, end = SourcePos {line = 1, column = 6, name = ""}}, number = IntegerNumber 123, typ = Nothing})), typ = Nothing}))))

apply :: SpecWith ()
apply = it
          "Apply"
          (do shouldBe
                (parseText "" "(x:x)(1)")
                (Right (ApplyExpression (Apply {location = SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}, function = LambdaExpression (Lambda {location = SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 5, name = ""}}, param = Param {location = SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}}, name = "x", typ = Nothing}, body = VariableExpression (Variable {location = SourceLocation {start = SourcePos {line = 1, column = 4, name = ""}, end = SourcePos {line = 1, column = 5, name = ""}}, name = "x", typ = Nothing}), typ = Nothing}), argument = LiteralExpression (NumberLiteral (Number {location = SourceLocation {start = SourcePos {line = 1, column = 7, name = ""}, end = SourcePos {line = 1, column = 8, name = ""}}, number = IntegerNumber 1, typ = Nothing})), typ = Nothing})))
              shouldBe
                (parseText "" "(x:y:1)(1,2)")
                (Right (ApplyExpression (Apply {location = SourceLocation {start = SourcePos {line = 1, column = 11, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}, function = ApplyExpression (Apply {location = SourceLocation {start = SourcePos {line = 1, column = 9, name = ""}, end = SourcePos {line = 1, column = 10, name = ""}}, function = LambdaExpression (Lambda {location = SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}, param = Param {location = SourceLocation {start = SourcePos {line = 1, column = 2, name = ""}, end = SourcePos {line = 1, column = 3, name = ""}}, name = "x", typ = Nothing}, body = LambdaExpression (Lambda {location = SourceLocation {start = SourcePos {line = 1, column = 4, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}, param = Param {location = SourceLocation {start = SourcePos {line = 1, column = 4, name = ""}, end = SourcePos {line = 1, column = 5, name = ""}}, name = "y", typ = Nothing}, body = LiteralExpression (NumberLiteral (Number {location = SourceLocation {start = SourcePos {line = 1, column = 6, name = ""}, end = SourcePos {line = 1, column = 7, name = ""}}, number = IntegerNumber 1, typ = Nothing})), typ = Nothing}), typ = Nothing}), argument = LiteralExpression (NumberLiteral (Number {location = SourceLocation {start = SourcePos {line = 1, column = 9, name = ""}, end = SourcePos {line = 1, column = 10, name = ""}}, number = IntegerNumber 1, typ = Nothing})), typ = Nothing}), argument = LiteralExpression (NumberLiteral (Number {location = SourceLocation {start = SourcePos {line = 1, column = 11, name = ""}, end = SourcePos {line = 1, column = 12, name = ""}}, number = IntegerNumber 2, typ = Nothing})), typ = Nothing}))))

sigs :: SpecWith ()
sigs =
  it
    "Signatures"
    (do shouldBe
          (parseText "" "123 :: Integer")
          (Right
             (LiteralExpression
                (NumberLiteral
                   (Number
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
                            , end = SourcePos {line = 1, column = 4, name = ""}
                            }
                      , number = IntegerNumber 123
                      , typ =
                          Just
                            (ConstantType
                               (TypeConstant
                                  { location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 8, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 15, name = ""}
                                        }
                                  , name = IntegerTypeName
                                  }))
                      }))))
        shouldBe
          (parseText "" "123 :: Decimal 1")
          (Right
             (LiteralExpression
                (NumberLiteral
                   (Number
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
                            , end = SourcePos {line = 1, column = 4, name = ""}
                            }
                      , number = IntegerNumber 123
                      , typ =
                          Just
                            (ApplyType
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
                                                       , column = 15
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
                                           , name = NatTypeName 1
                                           })
                                  , location =
                                      SourceLocation
                                        { start =
                                            SourcePos
                                              {line = 1, column = 8, name = ""}
                                        , end =
                                            SourcePos
                                              {line = 1, column = 17, name = ""}
                                        }
                                  , kind = TypeKind
                                  }))
                      })))))

records :: SpecWith ()
records =
  it
    "Record"
    (shouldBe
       (parseText "" "{a: 123+3/k, b: 452.2}")
       (Right
          (RecordExpression
             (Record
                { fields =
                    [ FieldE
                        { name = FieldName {unFieldName = "a"}
                        , expression =
                            InfixExpression
                              (Infix
                                 { location =
                                     SourceLocation
                                       { start =
                                           SourcePos
                                             {line = 1, column = 5, name = ""}
                                       , end =
                                           SourcePos
                                             {line = 1, column = 12, name = ""}
                                       }
                                 , global =
                                     Global
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
                                                   , column = 12
                                                   , name = ""
                                                   }
                                             }
                                       , name = "+"
                                       , scheme = ParsedScheme
                                       }
                                 , left =
                                     LiteralExpression
                                       (NumberLiteral
                                          (Number
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
                                                         , column = 8
                                                         , name = ""
                                                         }
                                                   }
                                             , number = IntegerNumber 123
                                             , typ = Nothing
                                             }))
                                 , right =
                                     InfixExpression
                                       (Infix
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
                                                      , column = 12
                                                      , name = ""
                                                      }
                                                }
                                          , global =
                                              Global
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
                                                            , column = 12
                                                            , name = ""
                                                            }
                                                      }
                                                , name = "/"
                                                , scheme = ParsedScheme
                                                }
                                          , left =
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
                                                      , number = IntegerNumber 3
                                                      , typ = Nothing
                                                      }))
                                          , right =
                                              VariableExpression
                                                (Variable
                                                   { location =
                                                       SourceLocation
                                                         { start =
                                                             SourcePos
                                                               { line = 1
                                                               , column = 11
                                                               , name = ""
                                                               }
                                                         , end =
                                                             SourcePos
                                                               { line = 1
                                                               , column = 12
                                                               , name = ""
                                                               }
                                                         }
                                                   , name = "k"
                                                   , typ = Nothing
                                                   })
                                          , typ = Nothing
                                          })
                                 , typ = Nothing
                                 })
                        , location =
                            SourceLocation
                              { start =
                                  SourcePos {line = 1, column = 3, name = ""}
                              , end =
                                  SourcePos {line = 1, column = 4, name = ""}
                              }
                        }
                    , FieldE
                        { name = FieldName {unFieldName = "b"}
                        , expression =
                            LiteralExpression
                              (NumberLiteral
                                 (Number
                                    { location =
                                        SourceLocation
                                          { start =
                                              SourcePos
                                                { line = 1
                                                , column = 17
                                                , name = ""
                                                }
                                          , end =
                                              SourcePos
                                                { line = 1
                                                , column = 22
                                                , name = ""
                                                }
                                          }
                                    , number =
                                        DecimalNumber
                                          (Decimal {places = 1, integer = 4522})
                                    , typ = Nothing
                                    }))
                        , location =
                            SourceLocation
                              { start =
                                  SourcePos {line = 1, column = 15, name = ""}
                              , end =
                                  SourcePos {line = 1, column = 16, name = ""}
                              }
                        }
                    ]
                , location =
                    SourceLocation
                      { start = SourcePos {line = 1, column = 1, name = ""}
                      , end = SourcePos {line = 1, column = 23, name = ""}
                      }
                , typ = Nothing
                }))))
