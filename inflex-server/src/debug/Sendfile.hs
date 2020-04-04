{-# LANGUAGE TemplateHaskell #-}

-- | Sendfile aware of DEBUG.

module Sendfile where

import Data.FileEmbed.Stack
import Language.Haskell.TH
import Yesod

sendFileFrom :: String -> FilePath -> Q Exp
sendFileFrom typ fp0 = do
  fp <- wrapStackRoot fp0
  [|sendFile typ fp|]
