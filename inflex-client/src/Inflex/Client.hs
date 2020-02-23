{-# LANGUAGE TemplateHaskell #-}
-- |

module Inflex.Client where

import Control.Monad
import Language.Haskell.TH.Syntax
import System.Directory
import System.Process.Typed

$(do runIO
       (do exists <- doesFileExist "app.js"
           unless exists (writeFile "app.js" ""))
     qAddDependentFile "app.js"
     runIO (do runProcess_ (proc "sh" ["bundle.sh"]))
     pure [])
