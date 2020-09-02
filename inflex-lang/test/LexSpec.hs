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
       (lexText "" "ab d_ex_f 123 456.1 123.456 12.000 ( )[]")
       (Right
          (Seq.fromList
             [ Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 1, name = ""}
                       , end = SourcePos {line = 1, column = 3, name = ""}
                       }
                 , thing = LowerWordToken "ab"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 4, name = ""}
                       , end = SourcePos {line = 1, column = 10, name = ""}
                       }
                 , thing = LowerWordToken "d_ex_f"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 11, name = ""}
                       , end = SourcePos {line = 1, column = 14, name = ""}
                       }
                 , thing = NaturalToken 123
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 15, name = ""}
                       , end = SourcePos {line = 1, column = 20, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 1, integer = 4561})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 21, name = ""}
                       , end = SourcePos {line = 1, column = 28, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 123456})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 29, name = ""}
                       , end = SourcePos {line = 1, column = 35, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 12000})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 36, name = ""}
                       , end = SourcePos {line = 1, column = 37, name = ""}
                       }
                 , thing = OpenRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 38, name = ""}
                       , end = SourcePos {line = 1, column = 39, name = ""}
                       }
                 , thing = CloseRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 39, name = ""}
                       , end = SourcePos {line = 1, column = 40, name = ""}
                       }
                 , thing = OpenSquareToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 40, name = ""}
                       , end = SourcePos {line = 1, column = 41, name = ""}
                       }
                 , thing = CloseSquareToken
                 }
             ])))
