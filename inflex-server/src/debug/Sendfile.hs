{-# LANGUAGE TemplateHaskell #-}

-- | Sendfile aware of DEBUG.

module Sendfile where

import qualified Data.ByteString as S
import           Data.FileEmbed.Stack
import qualified Data.Text.Encoding as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Yesod

sendFileFrom :: String -> FilePath -> Q Exp
sendFileFrom typ fp0 = do
  fp <- wrapStackRoot fp0
  addDependentFile fp
  [|sendFile typ fp|]

openFileFromBS :: FilePath -> Q Exp
openFileFromBS fp0 = do
  fp <- wrapStackRoot fp0
  addDependentFile fp
  [|(S.readFile fp)|]

openFileFromT :: FilePath -> Q Exp
openFileFromT fp0 = do
  fp <- wrapStackRoot fp0
  addDependentFile fp
  [|fmap T.decodeUtf8 (S.readFile fp)|]
