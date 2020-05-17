{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Tests for generation of type constraints.

module GenerateSpec where

import Inflex.Generator
import Inflex.Instances ()
import Inflex.Lexer
import Inflex.Types
import Test.Hspec

spec :: Spec
spec =
  it
    "Generate literal"
    (shouldBe
       (generateText "" "123")
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
