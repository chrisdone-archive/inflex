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
import           Test.QuickCheck

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
             "node run: roundtrip: MyRecord"
             (do property
                   (\i ->
                      let r = MyRecord {a = i}
                       in shouldReturn
                            (readProcessStdout_
                               (proc
                                  "node"
                                  [ "app.js"
                                  , "parse-and-encode"
                                  , L8.unpack (encode r)
                                  ]))
                            (encode r <> "\n")))))

data MyRecord = MyRecord { a :: Int }
 deriving (Generic, Show, Eq)
instance FromJSON MyRecord
instance ToJSON MyRecord
