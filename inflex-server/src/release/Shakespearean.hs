{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}

-- | RELEASE-aware code.

module Shakespearean
  ( luciusFileFrom
  ) where

import Data.FileEmbed.Stack
import Language.Haskell.TH
import Text.Lucius

luciusFileFrom :: FilePath -> ExpQ
luciusFileFrom fp = [|pure ($(wrapStackRoot fp >>= luciusFile) ())|]
