{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           GHC.Generics
import           System.Directory
import           System.Process.Typed
import           Test.Hspec

main :: IO ()
main = do
  setCurrentDirectory "test"
  hspec
    (describe
       "Compile"
       (do it
             "psc-package build: success"
             (shouldReturn
                (runProcess_
                   (proc "stack" ["exec", "--", "psc-package", "build"]))
                ())
           it
             "purs bundle: success"
             (shouldReturn
                (runProcess_
                   (proc
                      "stack"
                      [ "exec"
                      , "--"
                      , "psc-bundle-fast"
                      , "-i"
                      , "output"
                      , "-m"
                      , "Spec"
                      , "--main"
                      , "Spec"
                      , "-o"
                      , "app.js"
                      ]))
                ())
           it
             "node run: bijection"
             (do shouldReturn
                   (readProcessStdout_ (proc "node" ["app.js", "print"]))
                   "{\"a\":1}\n"
                 shouldBe (decode "{\"a\":1}\n") (pure (MyRecord {a = 1}))
                 shouldReturn
                   (readProcessStdout_
                      (proc
                         "node"
                         [ "app.js"
                         , "parse"
                         , L8.unpack (encode (MyRecord {a = 1}))
                         ]))
                   "(MyRecord { a: 1 })\n")))

data MyRecord = MyRecord { a :: Int }
 deriving (Generic, Show, Eq)
instance FromJSON MyRecord
instance ToJSON MyRecord
