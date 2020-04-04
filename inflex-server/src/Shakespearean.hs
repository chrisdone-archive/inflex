{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | DEBUG-aware code.

module Shakespearean
  ( luciusFileFrom
  ) where

import Language.Haskell.TH
import TH.RelativePaths
import Text.Lucius

luciusFileFrom :: FilePath -> Q Exp
#ifdef DEBUG
luciusFileFrom = luciusFileFromDebug
#else
luciusFileFrom = luciusFileFromRelease
#endif

luciusFileFromRelease :: FilePath -> ExpQ
luciusFileFromRelease fp0 =
  [|do url <- getUrlRender
       pure
         ($(do fp <- pathRelativeToCabalPackage fp0
               luciusFileReload fp)
            (\x _ -> url x))|]

luciusFileFromDebug :: FilePath -> ExpQ
luciusFileFromDebug fp = [|pure ($(luciusFile fp) ())|]
