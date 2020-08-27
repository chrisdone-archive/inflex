{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Server.Load where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.UUID.V4
import           Inflex.Display ()
import           Inflex.Document
import           Inflex.Instances ()
import           Inflex.Resolver
import qualified Inflex.Schema as Shared
import           Inflex.Types
import           Inflex.Types.Filler
import           Inflex.Types.Generator

loadInputDocument :: Shared.InputDocument -> Shared.OutputDocument
loadInputDocument inputDocument =
  undefined
  {-let loaded =
        loadDocument
          [ Named {uuid = Uuid u1, name = "x", thing = "y y"}
          , Named {uuid = Uuid u2, name = "y", thing = "1"}
          ]
   in evalDocument (evalEnvironment loaded) (defaultDocument loaded)-}
