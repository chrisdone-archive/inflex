{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.GenValidity
import           Data.GenValidity.Text ()
import           Data.GenValidity.Vector ()
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics
import           System.Directory
import           System.Process.Typed
import           Test.Hspec
import           Test.QuickCheck
import           Test.Validity

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
             (forAllUnchecked
                @MyRecord
                (\r ->
                   shouldReturn
                     (fmap
                        decode
                        (readProcessStdout_
                           (proc
                              "/home/chris/qjs"
                              ["app.js", "MyRecord", L8.unpack (encode r)])))
                     (pure r)))
           it
             "node run: roundtrip: MyRecord2"
             (forAllUnchecked
                @MyRecord2
                (\r ->
                   shouldReturn
                     (fmap
                        decode
                        (readProcessStdout_
                           (proc
                              "/home/chris/qjs"
                              ["app.js", "MyRecord2", L8.unpack (encode r)])))
                     (pure r)))))

data MyRecord = MyRecord { a :: Int }
 deriving (Generic, Show, Eq)
instance FromJSON MyRecord
instance ToJSON MyRecord
instance GenValid MyRecord
instance Validity MyRecord
instance GenUnchecked MyRecord

data MyRecord2 = MyRecord2 { b :: Int, myrec :: MyRecord, arr :: Vector MyRecord }
 deriving (Generic, Show, Eq)
instance FromJSON MyRecord2
instance ToJSON MyRecord2
instance GenValid MyRecord2
instance Validity MyRecord2
instance GenUnchecked MyRecord2
