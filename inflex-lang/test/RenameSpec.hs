{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module RenameSpec where

import Inflex.Instances ()
import Inflex.Lexer
import Inflex.Renamer
import Inflex.Types
import Test.Hspec

spec :: Spec
spec =
  it
    "Rename literal"
    (shouldBe
       (renameText "" "123")
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
                  , typ = ()
                  }))))
