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
       (lexText "" "AB ab d_ex_f 123 456.1 123.456 12.000 ( )[] {:,}")
       (Right
          (Seq.fromList
             [ Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 1, name = ""}
                       , end = SourcePos {line = 1, column = 3, name = ""}
                       }
                 , thing = AnyWordToken "AB"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 4, name = ""}
                       , end = SourcePos {line = 1, column = 6, name = ""}
                       }
                 , thing = CamelCaseToken "ab"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 7, name = ""}
                       , end = SourcePos {line = 1, column = 13, name = ""}
                       }
                 , thing = CamelCaseToken "d_ex_f"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 14, name = ""}
                       , end = SourcePos {line = 1, column = 17, name = ""}
                       }
                 , thing = NaturalToken 123
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 18, name = ""}
                       , end = SourcePos {line = 1, column = 23, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 1, integer = 4561})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 24, name = ""}
                       , end = SourcePos {line = 1, column = 31, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 123456})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 32, name = ""}
                       , end = SourcePos {line = 1, column = 38, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 12000})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 39, name = ""}
                       , end = SourcePos {line = 1, column = 40, name = ""}
                       }
                 , thing = OpenRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 41, name = ""}
                       , end = SourcePos {line = 1, column = 42, name = ""}
                       }
                 , thing = CloseRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 42, name = ""}
                       , end = SourcePos {line = 1, column = 43, name = ""}
                       }
                 , thing = OpenSquareToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 43, name = ""}
                       , end = SourcePos {line = 1, column = 44, name = ""}
                       }
                 , thing = CloseSquareToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 45, name = ""}
                       , end = SourcePos {line = 1, column = 46, name = ""}
                       }
                 , thing = OpenCurlyToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 46, name = ""}
                       , end = SourcePos {line = 1, column = 47, name = ""}
                       }
                 , thing = ColonToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 47, name = ""}
                       , end = SourcePos {line = 1, column = 48, name = ""}
                       }
                 , thing = CommaToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 48, name = ""}
                       , end = SourcePos {line = 1, column = 49, name = ""}
                       }
                 , thing = CloseCurlyToken
                 }
             ])))
