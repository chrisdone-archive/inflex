{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Shared.TH where

import           Control.Concurrent.Async
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

generateSchema :: Q [Dec]
generateSchema = do
  qAddDependentFile "config/schema"
  qAddDependentFile "templates/Shared.hs"
  qAddDependentFile "templates/Shared.purs"
  runIO
    (do content <- T.readFile "config/schema"
        concurrently_
          (do hs <- T.readFile "templates/Shared.hs"
              T.writeFile
                "../inflex-server/src/Inflex/Shared.hs"
                (T.replace "$types" content hs))
          (do purs <- T.readFile "templates/Shared.purs"
              T.writeFile
                "../inflex-client/src/Inflex/Shared.purs"
                (T.replace "$types" content purs))
        pure [])
