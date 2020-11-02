{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- | DEBUG-aware code.

module Shakespearean
  ( luciusFileFrom
  , juliusFileFrom
  ) where

import Data.FileEmbed.Stack
import Language.Haskell.TH
import Text.Lucius
import Text.Julius
import Text.RawString.QQ

luciusFileFrom :: FilePath -> ExpQ
luciusFileFrom fp0 =
  [|do url <- getUrlRender
       pure
         ($(wrapStackRoot fp0 >>= luciusFileReload)
            (\x _ -> url x))|]

juliusFileFrom :: FilePath -> ExpQ
juliusFileFrom fp0 =
  [|do url <- getUrlRender
       pure
         ($(wrapStackRoot fp0 >>= juliusFileReload)
            (\x _ -> url x))|]
