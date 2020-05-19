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
                       , types = VariableType IntegeryPrefix 0 :| []
                       }
                   ]
             , thing =
                 LiteralExpression
                   (IntegerLiteral
                      (Integery
                         { location =
                             Location
                               { start =
                                   SourcePos {line = 1, column = 1, name = ""}
                               , end =
                                   SourcePos {line = 1, column = 4, name = ""}
                               }
                         , integer = 123
                         , typ = VariableType IntegeryPrefix 0
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
                       , types = VariableType IntegeryPrefix 1 :| []
                       }
                   ]
             , thing =
                 LambdaExpression
                   (Lambda
                      { location =
                          Location
                            { start =
                                SourcePos {line = 1, column = 1, name = ""}
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
                                  , typ = VariableType IntegeryPrefix 1
                                  }))
                      , typ =
                          ApplyType
                            (ApplyType
                               (ConstantType FunctionTypeName)
                               (VariableType LambdaParameterPrefix 0))
                            (VariableType IntegeryPrefix 1)
                      })
             })))
