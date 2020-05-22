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
                               , location = FinalCursor
                               } :|
                           []
                       , location = FinalCursor
                       }
                   ]
             , thing =
                 LiteralExpression
                   (IntegerLiteral
                      (Integery
                         { location = FinalCursor
                         , integer = 123
                         , typ =
                             VariableType
                               TypeVariable
                                 { prefix = IntegeryPrefix
                                 , index = 0
                                 , location = FinalCursor
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
                                { location = InLambdaCursor FinalCursor
                                , prefix = IntegeryPrefix
                                , index = 1
                                }) :|
                           []
                       , location = InLambdaCursor FinalCursor
                       }
                   ]
             , thing =
                 LambdaExpression
                   (Lambda
                      { location = FinalCursor
                      , body =
                          LiteralExpression
                            (IntegerLiteral
                               (Integery
                                  { location = InLambdaCursor FinalCursor
                                  , integer = 123
                                  , typ =
                                      VariableType
                                        (TypeVariable
                                           { location =
                                               InLambdaCursor FinalCursor
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
                                                 { location = FinalCursor
                                                 , name = FunctionTypeName
                                                 })
                                        , argument =
                                            VariableType
                                              (TypeVariable
                                                 { location = FinalCursor
                                                 , prefix =
                                                     LambdaParameterPrefix
                                                 , index = 0
                                                 })
                                        , location = FinalCursor
                                        })
                               , argument =
                                   VariableType
                                     (TypeVariable
                                        { location = InLambdaCursor FinalCursor
                                        , prefix = IntegeryPrefix
                                        , index = 1
                                        })
                               , location = FinalCursor
                               })
                      })
             })))
