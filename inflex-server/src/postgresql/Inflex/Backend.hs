{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |

module Inflex.Backend (module X, withBackendPool,manualMigration) where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.Functor
import Database.Persist.Postgresql as X
import Yesod

withBackendPool = withPostgresqlPool

manualMigration ::
  (MonadIO m, BackendCompatible SqlBackend backend) =>
  ReaderT backend m ()
manualMigration = when False (void (rawExecute "" []))
