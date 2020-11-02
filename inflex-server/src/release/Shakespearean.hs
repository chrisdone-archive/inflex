{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}

-- | RELEASE-aware code.

module Shakespearean
  ( luciusFileFrom
  , juliusFileFrom
  ) where

import Data.FileEmbed.Stack
import Language.Haskell.TH
import Text.Lucius
import Text.Julius

luciusFileFrom :: FilePath -> ExpQ
luciusFileFrom fp =
  [|do urlRenderer <- getUrlRender
       pure ($(wrapStackRoot fp >>= luciusFile) (\route _ -> urlRenderer route))|]

juliusFileFrom :: FilePath -> ExpQ
juliusFileFrom fp =
  [|do urlRenderer <- getUrlRender
       pure ($(wrapStackRoot fp >>= juliusFile) (\route _ -> urlRenderer route))|]
