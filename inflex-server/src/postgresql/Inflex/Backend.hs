{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Backend driver.

module Inflex.Backend (module X, withBackendPool) where

import           Database.Persist
import           Database.Persist.Postgresql as X

withBackendPool = withPostgresqlPool
