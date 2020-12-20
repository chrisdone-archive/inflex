{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper to easily compile Inflex code at compile-time.

module Inflex.Derived.TH where

import           Data.Text (Text)
import           Inflex.Resolver
import           Language.Haskell.TH.Syntax
import qualified RIO

compile :: Text -> Q Exp
compile src = do
  result <- runIO (RIO.runRIO ResolveReader (resolveText mempty "Derived" src))
  case result of
    Left (err :: GeneraliseResolveError ()) -> error (show err)
    Right IsResolved {thing} -> lift thing
