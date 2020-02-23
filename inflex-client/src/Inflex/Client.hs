{-# LANGUAGE TemplateHaskell #-}
-- |

module Inflex.Client where

import Control.Monad
import Language.Haskell.TH.Syntax
import System.Directory
import System.Process.Typed

$(runIO
    (do runProcess_ (proc "sh" ["bundle.sh"])
        pure []))
