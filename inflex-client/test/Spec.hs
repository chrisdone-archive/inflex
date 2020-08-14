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
import           GHC.Generics
import           System.Directory
import           System.Process.Typed
import           Test.Hspec
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
                     (pure r)))
           it
             "node run: roundtrip: MyProductType1"
             (forAllUnchecked
                @MyProductType1
                (\r ->
                   shouldReturn
                     (fmap
                        decode
                        (readProcessStdout_
                           (proc
                              "/home/chris/qjs"
                              ["app.js", "MyProductType1", L8.unpack (encode r)])))
                     (pure r)))
           it
             "node run: roundtrip: MyProductType2"
             (forAllUnchecked
                @MyProductType2
                (\r ->
                   shouldReturn
                     (fmap
                        decode
                        (readProcessStdout_
                           (proc
                              "/home/chris/qjs"
                              ["app.js", "MyProductType2", L8.unpack (encode r)])))
                     (pure r)))
           it
             "node run: roundtrip: MyProductType3"
             (forAllUnchecked
                @MyProductType3
                (\r ->
                   shouldReturn
                     (fmap
                        decode
                        (readProcessStdout_
                           (proc
                              "/home/chris/qjs"
                              ["app.js", "MyProductType3", L8.unpack (encode r)])))
                     (pure r)))))

data MyRecord = MyRecord
  { a :: Int
  } deriving (Generic, Show, Eq)
instance FromJSON MyRecord
instance ToJSON MyRecord
instance GenValid MyRecord
instance Validity MyRecord
instance GenUnchecked MyRecord

data MyRecord2 = MyRecord2
  { b :: Int
  , myrec :: MyRecord
  , mrec :: Maybe MyRecord
  , arr :: Vector MyRecord
  } deriving (Generic, Show, Eq)
instance FromJSON MyRecord2
instance ToJSON MyRecord2
instance GenValid MyRecord2
instance Validity MyRecord2
instance GenUnchecked MyRecord2

data MyProductType1 = MyProductType1
  Int
   deriving (Generic, Show, Eq)
instance FromJSON MyProductType1
instance ToJSON MyProductType1
instance GenValid MyProductType1
instance Validity MyProductType1
instance GenUnchecked MyProductType1

data MyProductType2 = MyProductType2
  Int Int
   deriving (Generic, Show, Eq)
instance FromJSON MyProductType2
instance ToJSON MyProductType2
instance GenValid MyProductType2
instance Validity MyProductType2
instance GenUnchecked MyProductType2

data MyProductType3 = MyProductType3
  Int MyProductType2
   deriving (Generic, Show, Eq)
instance FromJSON MyProductType3
instance ToJSON MyProductType3
instance GenValid MyProductType3
instance Validity MyProductType3
instance GenUnchecked MyProductType3
