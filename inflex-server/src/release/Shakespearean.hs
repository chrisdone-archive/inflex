{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}

-- | RELEASE-aware code.

module Shakespearean
  ( luciusFileFrom
  ) where

import Data.FileEmbed.Stack
import Language.Haskell.TH
import Text.Lucius

luciusFileFrom :: FilePath -> ExpQ
luciusFileFrom fp =
  [|do urlRenderer <- getUrlRender
       pure ($(wrapStackRoot fp >>= luciusFile) (\route _ -> urlRenderer route))|]
