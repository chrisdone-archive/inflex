{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Test the lexer.

module LexSpec where

import qualified Data.Sequence as Seq
import           Inflex.Lexer
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec =
  it
    "Tokens"
    (shouldBe
       (lexText "" "a 123 456.1 123.456 12.000 ( )[]")
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
                       , end = SourcePos {line = 1, column = 12, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 1, integer = 4561})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 13, name = ""}
                       , end = SourcePos {line = 1, column = 20, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 123456})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 21, name = ""}
                       , end = SourcePos {line = 1, column = 27, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 12000})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 28, name = ""}
                       , end = SourcePos {line = 1, column = 29, name = ""}
                       }
                 , thing = OpenRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 30, name = ""}
                       , end = SourcePos {line = 1, column = 31, name = ""}
                       }
                 , thing = CloseRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 31, name = ""}
                       , end = SourcePos {line = 1, column = 32, name = ""}
                       }
                 , thing = OpenSquareToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 32, name = ""}
                       , end = SourcePos {line = 1, column = 33, name = ""}
                       }
                 , thing = CloseSquareToken
                 }
             ])))
