{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Data.Bifunctor
import qualified Duet.Types as D
import qualified Language.PureScript.AST.Declarations as P
import qualified Language.PureScript.CST as CST
import           Test.Hspec

main :: IO ()
main =
  hspec
    (it
       "Parse parity"
       (shouldBe
          (bimap (const ()) (const ()) (CST.parseFromFile "" ""))
          (Left ())))

fromDecls :: [D.Decl D.UnkindedType D.Identifier D.Location] -> [P.Declaration]
fromDecls = fmap fromDecl

fromDecl :: D.Decl D.UnkindedType D.Identifier D.Location -> P.Declaration
fromDecl = undefined
