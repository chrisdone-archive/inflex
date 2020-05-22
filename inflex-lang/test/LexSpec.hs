{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Test the lexer.

module LexSpec where

import qualified Data.Sequence as Seq
import           Inflex.Lexer
import           Test.Hspec

spec :: Spec
spec =
  it
    "Tokens"
    (shouldBe
       (lexText "" "a 123 ( )[]")
       (Right
          (Seq.fromList
             [ Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 1, name = ""}
                       , end = SourcePos {line = 1, column = 2, name = ""}
                       }
                 , thing = LowerWordToken "a"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 3, name = ""}
                       , end = SourcePos {line = 1, column = 6, name = ""}
                       }
                 , thing = IntegerToken 123
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 7, name = ""}
                       , end = SourcePos {line = 1, column = 8, name = ""}
                       }
                 , thing = OpenRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 9, name = ""}
                       , end = SourcePos {line = 1, column = 10, name = ""}
                       }
                 , thing = CloseRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 10, name = ""}
                       , end = SourcePos {line = 1, column = 11, name = ""}
                       }
                 , thing = OpenSquareToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 11, name = ""}
                       , end = SourcePos {line = 1, column = 12, name = ""}
                       }
                 , thing = CloseSquareToken
                 }
             ])))
