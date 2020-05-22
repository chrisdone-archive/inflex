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
                               , location = Cursor
                               } :|
                           []
                       , location = Cursor
                       }
                   ]
             , thing =
                 LiteralExpression
                   (IntegerLiteral
                      (Integery
                         { location = Cursor
                         , integer = 123
                         , typ =
                             VariableType
                               TypeVariable
                                 { prefix = IntegeryPrefix
                                 , index = 0
                                 , location = Cursor
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
                                { location = Cursor
                                , prefix = IntegeryPrefix
                                , index = 1
                                }) :|
                           []
                       , location = Cursor
                       }
                   ]
             , thing =
                 LambdaExpression
                   (Lambda
                      { location = Cursor
                      , body =
                          LiteralExpression
                            (IntegerLiteral
                               (Integery
                                  { location = Cursor
                                  , integer = 123
                                  , typ =
                                      VariableType
                                        (TypeVariable
                                           { location = Cursor
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
                                                 { location = Cursor
                                                 , name = FunctionTypeName
                                                 })
                                        , argument =
                                            VariableType
                                              (TypeVariable
                                                 { location = Cursor
                                                 , prefix =
                                                     LambdaParameterPrefix
                                                 , index = 0
                                                 })
                                        , location = Cursor
                                        })
                               , argument =
                                   VariableType
                                     (TypeVariable
                                        { location = Cursor
                                        , prefix = IntegeryPrefix
                                        , index = 1
                                        })
                               , location = Cursor
                               })
                      })
             })))
