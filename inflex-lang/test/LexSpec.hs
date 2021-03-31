{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Test the lexer.

module LexSpec where

import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.UUID as UUID
import           Inflex.Lexer
import           Inflex.Types
import           Test.Hspec

spec :: Spec
spec =
  it
    "Tokens"
    (shouldBe
       (lexText
          ""
          "@uuid:1ea653f3-67f7-4fad-9892-85ce6cbf10a7 \
          \AB ab d_ex_f 123 456.1 123.456 12.000 ( )[] {:,} \
          \@sha512:3ba402f10ef7807ab8767a44d57ed1b6dcfc84d629219a0603535993c93b6279ecb4aab48763b5b84b8c45d9ea2b90bf7356e06b063cc4478f2b817d66f449ad")
       (Right
          (Seq.fromList
             [ Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 1, name = ""}
                       , end = SourcePos {line = 1, column = 43, name = ""}
                       }
                 , thing =
                     RefToken
                       (UuidRef
                          (fromJust
                             (UUID.fromString
                                "1ea653f3-67f7-4fad-9892-85ce6cbf10a7")))
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 44, name = ""}
                       , end = SourcePos {line = 1, column = 46, name = ""}
                       }
                 , thing = AnyWordToken "AB"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 47, name = ""}
                       , end = SourcePos {line = 1, column = 49, name = ""}
                       }
                 , thing = CamelCaseToken "ab"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 50, name = ""}
                       , end = SourcePos {line = 1, column = 56, name = ""}
                       }
                 , thing = CamelCaseToken "d_ex_f"
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 57, name = ""}
                       , end = SourcePos {line = 1, column = 60, name = ""}
                       }
                 , thing = NaturalToken 123
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 61, name = ""}
                       , end = SourcePos {line = 1, column = 66, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 1, integer = 4561})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 67, name = ""}
                       , end = SourcePos {line = 1, column = 74, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 123456})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 75, name = ""}
                       , end = SourcePos {line = 1, column = 81, name = ""}
                       }
                 , thing = DecimalToken (Decimal {places = 3, integer = 12000})
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 82, name = ""}
                       , end = SourcePos {line = 1, column = 83, name = ""}
                       }
                 , thing = OpenRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 84, name = ""}
                       , end = SourcePos {line = 1, column = 85, name = ""}
                       }
                 , thing = CloseRoundToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 85, name = ""}
                       , end = SourcePos {line = 1, column = 86, name = ""}
                       }
                 , thing = OpenSquareToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 86, name = ""}
                       , end = SourcePos {line = 1, column = 87, name = ""}
                       }
                 , thing = CloseSquareToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 88, name = ""}
                       , end = SourcePos {line = 1, column = 89, name = ""}
                       }
                 , thing = OpenCurlyToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 89, name = ""}
                       , end = SourcePos {line = 1, column = 90, name = ""}
                       }
                 , thing = ColonToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 90, name = ""}
                       , end = SourcePos {line = 1, column = 91, name = ""}
                       }
                 , thing = CommaToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 91, name = ""}
                       , end = SourcePos {line = 1, column = 92, name = ""}
                       }
                 , thing = CloseCurlyToken
                 }
             , Located
                 { location =
                     SourceLocation
                       { start = SourcePos {line = 1, column = 93, name = ""}
                       , end = SourcePos {line = 1, column = 229, name = ""}
                       }
                 , thing =
                     RefToken
                       (Sha512Ref
                          $$("3ba402f10ef7807ab8767a44d57ed1b6dcfc84d629219a0603535993c93b6279"))
                 }
             ])))
