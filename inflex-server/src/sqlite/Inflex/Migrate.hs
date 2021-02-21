{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |

module Inflex.Migrate (manualMigration) where

import           Database.Persist.Sqlite as X

manualMigration _ = runMigration
