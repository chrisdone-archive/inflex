{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper to easily compile Inflex code at compile-time.

module Inflex.Derived.TH where

import           Data.Text (Text)
import           Inflex.Resolver
import           Language.Haskell.TH.Syntax

compile :: Text -> Q Exp
compile src =
  case resolveText mempty "Derived" src of
    Left (err :: GeneraliseResolveError ()) -> error (show err)
    Right IsResolved {thing} -> lift thing
