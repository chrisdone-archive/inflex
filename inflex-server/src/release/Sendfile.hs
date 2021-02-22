{-# LANGUAGE TemplateHaskell #-}

-- | Sendfile aware of RELEASE.

module Sendfile where

import           Data.FileEmbed
import           Data.FileEmbed.Stack
import qualified Data.Text.Encoding as T
import           Language.Haskell.TH
import           Yesod

sendFileFrom :: String -> FilePath -> Q Exp
sendFileFrom typ fp0 = do
  fp <- wrapStackRoot fp0
  bs <- embedFile fp
  [|pure (Yesod.TypedContent typ (toContent $(pure bs)))|]

openFileFromBS :: FilePath -> Q Exp
openFileFromBS fp0 = do
  fp <- wrapStackRoot fp0
  bs <- embedFile fp
  [|pure $(pure bs)|]

openFileFromT :: FilePath -> Q Exp
openFileFromT fp0 = do
  fp <- wrapStackRoot fp0
  bs <- embedFile fp
  [|pure $ T.decodeUtf8 $(pure bs)|]
