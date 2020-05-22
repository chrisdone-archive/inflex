{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Tests for generation of type constraints.

module GenerateSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.Lexer
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (generateText "" "123")
       (Right
          (HasConstraints
             { classes =
                 Seq.fromList
                   [ ClassConstraint
                       { className = FromIntegerClassName
                       , types =
                           VariableType
                             TypeVariable
                               { prefix = IntegeryPrefix
                               , index = 0
                               , location =
                                   SourceLocation
                                     { start =
                                         SourcePos
                                           {line = 1, column = 1, name = ""}
                                     , end =
                                         SourcePos
                                           {line = 1, column = 4, name = ""}
                                     }
                               } :|
                           []
                       , location =
                           SourceLocation
                             { start =
                                 SourcePos {line = 1, column = 1, name = ""}
                             , end = SourcePos {line = 1, column = 4, name = ""}
                             }
                       }
                   ]
             , thing =
                 LiteralExpression
                   (IntegerLiteral
                      (Integery
                         { location =
                             SourceLocation
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 4, name = ""}
                               }
                         , integer = 123
                         , typ =
                             VariableType
                               TypeVariable
                                 { prefix = IntegeryPrefix
                                 , index = 0
                                 , location =
                                     SourceLocation
                                       { start =
                                           SourcePos
                                             {line = 1, column = 1, name = ""}
                                       , end =
                                           SourcePos
                                             {line = 1, column = 4, name = ""}
                                       }
                                 }
                         }))
             })))
  it
    "Lambda"
    (shouldBe
       (generateText "" "\\->123")
       (Right
          (HasConstraints
             { classes =
                 Seq.fromList
                   [ ClassConstraint
                       { className = FromIntegerClassName
                       , types =
                           VariableType
                             (TypeVariable
                                { location =
                                    SourceLocation
                                      { start =
                                          SourcePos
                                            {line = 1, column = 4, name = ""}
                                      , end =
                                          SourcePos
                                            {line = 1, column = 7, name = ""}
                                      }
                                , prefix = IntegeryPrefix
                                , index = 1
                                }) :|
                           []
                       , location =
                           SourceLocation
                             { start =
                                 SourcePos {line = 1, column = 4, name = ""}
                             , end = SourcePos {line = 1, column = 7, name = ""}
                             }
                       }
                   ]
             , thing =
                 LambdaExpression
                   (Lambda
                      { location =
                          SourceLocation
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
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
                                  , typ =
                                      VariableType
                                        (TypeVariable
                                           { location =
                                               SourceLocation
                                                 { start =
                                                     SourcePos
                                                       { line = 1
                                                       , column = 4
                                                       , name = ""
                                                       }
                                                 , end =
                                                     SourcePos
                                                       { line = 1
                                                       , column = 7
                                                       , name = ""
                                                       }
                                                 }
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
                                                             , column = 7
                                                             , name = ""
                                                             }
                                                       }
                                                 , name = FunctionTypeName
                                                 })
                                        , argument =
                                            VariableType
                                              (TypeVariable
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
                                                             , column = 7
                                                             , name = ""
                                                             }
                                                       }
                                                 , prefix =
                                                     LambdaParameterPrefix
                                                 , index = 0
                                                 })
                                        , location =
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
                                                    , column = 7
                                                    , name = ""
                                                    }
                                              }
                                        })
                               , argument =
                                   VariableType
                                     (TypeVariable
                                        { location =
                                            SourceLocation
                                              { start =
                                                  SourcePos
                                                    { line = 1
                                                    , column = 4
                                                    , name = ""
                                                    }
                                              , end =
                                                  SourcePos
                                                    { line = 1
                                                    , column = 7
                                                    , name = ""
                                                    }
                                              }
                                        , prefix = IntegeryPrefix
                                        , index = 1
                                        })
                               , location =
                                   SourceLocation
                                     { start =
                                         SourcePos
                                           {line = 1, column = 1, name = ""}
                                     , end =
                                         SourcePos
                                           {line = 1, column = 7, name = ""}
                                     }
                               })
                      })
             })))
