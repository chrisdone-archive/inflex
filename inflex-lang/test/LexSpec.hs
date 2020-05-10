{-# LANGUAGE OverloadedStrings #-}

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
                 { start = SourcePos {line = 1, column = 1}
                 , end = SourcePos {line = 1, column = 1}
                 , thing = LowerWordToken "a"
                 }
             , Located
                 { start = SourcePos {line = 1, column = 1}
                 , end = SourcePos {line = 1, column = 1}
                 , thing = IntegerToken 123
                 }
             , Located
                 { start = SourcePos {line = 1, column = 1}
                 , end = SourcePos {line = 1, column = 1}
                 , thing = OpenRoundToken
                 }
             , Located
                 { start = SourcePos {line = 1, column = 1}
                 , end = SourcePos {line = 1, column = 1}
                 , thing = CloseRoundToken
                 }
             , Located
                 { start = SourcePos {line = 1, column = 1}
                 , end = SourcePos {line = 1, column = 1}
                 , thing = OpenSquareToken
                 }
             , Located
                 { start = SourcePos {line = 1, column = 1}
                 , end = SourcePos {line = 1, column = 1}
                 , thing = CloseSquareToken
                 }
             ])))
