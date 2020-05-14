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
    "Integer literal"
    (shouldBe
       (lexText "" "a 123 ( )[]")
       (Right
          (Seq.fromList
             [ Located
                 { location =
                     Location
                       { start = SourcePos {name = "", line = 1, column = 1}
                       , end = SourcePos {name = "", line = 1, column = 1}
                       }
                 , thing = LowerWordToken "a"
                 }
             , Located
                 { location =
                     Location
                       { start = SourcePos {name = "", line = 1, column = 1}
                       , end = SourcePos {name = "", line = 1, column = 1}
                       }
                 , thing = IntegerToken 123
                 }
             , Located
                 { location =
                     Location
                       { start = SourcePos {name = "", line = 1, column = 1}
                       , end = SourcePos {name = "", line = 1, column = 1}
                       }
                 , thing = OpenRoundToken
                 }
             , Located
                 { location =
                     Location
                       { start = SourcePos {name = "", line = 1, column = 1}
                       , end = SourcePos {name = "", line = 1, column = 1}
                       }
                 , thing = CloseRoundToken
                 }
             , Located
                 { location =
                     Location
                       { start = SourcePos {name = "", line = 1, column = 1}
                       , end = SourcePos {name = "", line = 1, column = 1}
                       }
                 , thing = OpenSquareToken
                 }
             , Located
                 { location =
                     Location
                       { start = SourcePos {name = "", line = 1, column = 1}
                       , end = SourcePos {name = "", line = 1, column = 1}
                       }
                 , thing = CloseSquareToken
                 }
             ])))
