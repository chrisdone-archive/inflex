{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Vector (Vector)
import qualified Data.Vector as V
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
                            (fmap
                               decode
                               (readProcessStdout_
                                  (proc
                                     "/home/chris/qjs"
                                     [ "app.js"
                                     , "MyRecord"
                                     , L8.unpack (encode r)
                                     ])))
                            (pure r)))
           it
             "node run: roundtrip: MyRecord2"
             (do property
                   (\i ->
                      let r =
                            MyRecord2
                              { b = i
                              , myrec = MyRecord {a = i * 2}
                              , arr =
                                  V.replicate (mod i 10) (MyRecord {a = i * 3})
                              }
                       in shouldReturn
                            (fmap
                               decode
                               (readProcessStdout_
                                  (proc
                                     "/home/chris/qjs"
                                     [ "app.js"
                                     , "MyRecord2"
                                     , L8.unpack (encode r)
                                     ])))
                            (pure r)))))

data MyRecord = MyRecord { a :: Int }
 deriving (Generic, Show, Eq)
instance FromJSON MyRecord
instance ToJSON MyRecord

data MyRecord2 = MyRecord2 { b :: Int, myrec :: MyRecord, arr :: Vector MyRecord }
 deriving (Generic, Show, Eq)
instance FromJSON MyRecord2
instance ToJSON MyRecord2
