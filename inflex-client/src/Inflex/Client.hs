{-# LANGUAGE TemplateHaskell #-}

-- |

module Inflex.Client where

import Language.Haskell.TH.Syntax
import Stack
import System.Process.Typed

$(do fp <- makeRelativeToProject "inflex-client/"
     runIO (runProcess_ (setWorkingDir fp (proc "sh" ["bundle-full.sh"])))
     pure [])
