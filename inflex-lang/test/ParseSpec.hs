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
          (IntegerLiteral
             Integery
               { location =
                   Location
                     { start = SourcePos {name = "", line = 1, column = 1}
                     , end = SourcePos {name = "", line = 1, column = 1}
                     }
               , integer = 123
               })))
