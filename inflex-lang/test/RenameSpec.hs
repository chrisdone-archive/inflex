{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module RenameSpec where

import Inflex.Instances ()
import Inflex.Renamer
import Inflex.Types
import Test.Hspec

spec :: Spec
spec = do
  it
    "Literal"
    (shouldBe
       (renameText "" "123")
       (Right
          (LiteralExpression
             (IntegerLiteral
                Integery {location = Cursor, integer = 123, typ = ()}))))
  it
    "Lambda"
    (shouldBe
       (renameText "" "\\->123")
       (Right
          (LambdaExpression
             (Lambda
                { typ = ()
                , location = Cursor
                , body =
                    LiteralExpression
                      (IntegerLiteral
                         (Integery {location = Cursor, integer = 123, typ = ()}))
                }))))
