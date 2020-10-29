{-# LANGUAGE TemplateHaskell #-}

-- | Sendfile aware of DEBUG.

module Sendfile where

import           Data.FileEmbed.Stack
import qualified Data.Text.IO as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Yesod

sendFileFrom :: String -> FilePath -> Q Exp
sendFileFrom typ fp0 = do
  fp <- wrapStackRoot fp0
  addDependentFile fp
  [|sendFile typ fp|]

openFileFrom :: FilePath -> Q Exp
openFileFrom fp0 = do
  fp <- wrapStackRoot fp0
  addDependentFile fp
  [|T.readFile fp|]
