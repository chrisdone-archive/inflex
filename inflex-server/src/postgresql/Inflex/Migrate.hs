{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Migrations.

module Inflex.Migrate where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Reader
import           Data.String
import           Data.String.Quote
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Database.Persist.Postgresql as X
import           Stripe
import           Yesod

--------------------------------------------------------------------------------
-- Latest migration

latestVersion :: Int
latestVersion = 11

migrateToLatest :: MonadIO m => ReaderT SqlBackend m ()
migrateToLatest  = run [s|

INSERT INTO schema_versions VALUES (1);

-- mutations

ALTER TABLE "revision_cell" ADD COLUMN "x" INT8 NULL;
ALTER TABLE "revision_cell" ADD COLUMN "y" INT8 NULL;

|]

skipPersistent :: IsString a => [a]
skipPersistent =
  [
  ]

--------------------------------------------------------------------------------
-- Migration harness

manualMigration :: (MonadIO m, MonadThrow m) => StripeConfig -> x -> ReaderT SqlBackend m ()
manualMigration _stripeConfig _x
  -- Set isolation, and validate it.
 = do
  run "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;"
  level <-
    fmap
      (fmap unSingle)
      (rawSql "SELECT CURRENT_SETTING('transaction_isolation');" [])
  unless
    (level == ["serializable" :: Text])
    (error "Transaction isolation level is not being set properly!")
  -- Setup schema versions table.
  run
    "CREATE TABLE IF NOT EXISTS schema_versions (version INT, date TIMESTAMP DEFAULT NOW());"
  -- Dispatch a migration.
  version :: Int <-
    fmap
      (foldl (+) 0 . fmap unSingle)
      (rawSql "SELECT version FROM schema_versions;" [])
  liftIO $ putStrLn ("Schema version: " ++ show version)
  if version == latestVersion
    then liftIO $ putStrLn "OK, at latest version."
    else if version == latestVersion - 1
           then do
             liftIO $
               putStrLn
                 ("Migrating from " ++
                  show version ++ " to " ++ show version ++ " ...")
             migrateToLatest
             liftIO $ putStrLn "Done!"
           else error ("At mysterious schema version: " ++ show version)

run :: (MonadIO m) => Text -> ReaderT SqlBackend m ()
run x = do
  liftIO $ T.putStrLn x
  void $ rawExecute x []
