{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test the parser.

module ParseSpec where

import Inflex.Lexer
import Inflex.Parser
import Test.Hspec

spec :: Spec
spec =
  it
    "Integer literal"
    (shouldBe
       (parseText "" "123")
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
                  }))))
