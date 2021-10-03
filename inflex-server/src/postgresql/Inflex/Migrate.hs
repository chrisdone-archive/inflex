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
latestVersion = 12

migrateToLatest :: MonadIO m => ReaderT SqlBackend m ()
migrateToLatest  = run [s|
INSERT INTO schema_versions VALUES (1);

DROP TABLE file;
CREATE TABLE "file"("id" SERIAL8  PRIMARY KEY UNIQUE,"account" INT8 NOT NULL,"name" VARCHAR NOT NULL,"created" TIMESTAMP WITH TIME ZONE NOT NULL,"hash" BYTEA NOT NULL,"bytes" INT8 NOT NULL,"mime" VARCHAR NOT NULL,"content" BYTEA NOT NULL);
ALTER TABLE "file" ADD CONSTRAINT "file_account_fkey" FOREIGN KEY("account") REFERENCES "account"("id");
-- requires PG-12.8, that's what the server supports. my local desktop version is 10
ALTER TABLE file SET (toast_tuple_target=128);

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
                  show version ++ " to " ++ show latestVersion ++ " ...")
             migrateToLatest
             liftIO $ putStrLn "Done!"
           else error ("At mysterious schema version: " ++ show version)

run :: (MonadIO m) => Text -> ReaderT SqlBackend m ()
run x = do
  liftIO $ T.putStrLn x
  void $ rawExecute x []
