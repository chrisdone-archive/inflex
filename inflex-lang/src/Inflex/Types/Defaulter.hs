-- |

module Inflex.Types.Defaulter where

import Inflex.Types
import Inflex.Types.Resolver

--------------------------------------------------------------------------------
-- Types

data DefaulterError
  = ResolutionError ResolutionError
  | DefaultingNoInstanceFound (ClassConstraint Polymorphic)
  deriving (Eq, Show)

data ResolverDefaulterError e
  = DefaulterError DefaulterError
  | GeneraliseResolverError (GeneraliseResolveError e)
  deriving (Eq, Show)
