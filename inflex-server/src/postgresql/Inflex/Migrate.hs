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
import           Data.String.Quote
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Database.Persist.Postgresql as X
import           Stripe
import           Yesod

--------------------------------------------------------------------------------
-- Latest migration

latestVersion :: Int
latestVersion = 10

migrateToLatest :: MonadIO m => ReaderT SqlBackend m ()
migrateToLatest  = run [s|

INSERT INTO schema_versions VALUES (1);

-- mutations

ALTER TABLE "document" ADD COLUMN "revision" INT8 NULL;

ALTER TABLE "revision" DROP COLUMN "content";
ALTER TABLE "revision" DROP COLUMN "active";
ALTER TABLE "revision" DROP COLUMN "activated";

-- new tables

CREATe TABLE "revision_cell"("id" SERIAL8  PRIMARY KEY UNIQUE,"revision" INT8 NOT NULL,"cell" INT8 NOT NULL,"order" INT8 NOT NULL);

CREATe TABLE "cell"("id" SERIAL8  PRIMARY KEY UNIQUE,"account" INT8 NOT NULL,"document" INT8 NOT NULL,"code" INT8 NOT NULL,"created" TIMESTAMP WITH TIME ZONE NOT NULL,"name" VARCHAR NOT NULL,"uuid" VARCHAR NOT NULL) WITH (toast_tuple_target=128);

CREATe TABLE "code"("id" SERIAL8  PRIMARY KEY UNIQUE,"source" VARCHAR NOT NULL,"hash" BYTEA NOT NULL,"created" TIMESTAMP WITH TIME ZONE NOT NULL) WITH (toast_tuple_target=128);

-- constraints

delete from file where account not in (select id from account);
delete from revision where account not in (select id from account) or document not in (select id from document);

ALTER TABLE "revision_cell" ADD CONSTRAINT "revision_cell_revision_fkey" FOREIGN KEY("revision") REFERENCES "revision"("id");
ALTER TABLE "revision_cell" ADD CONSTRAINT "revision_cell_cell_fkey" FOREIGN KEY("cell") REFERENCES "cell"("id");

ALTER TABLE "cell" ADD CONSTRAINT "cell_account_fkey" FOREIGN KEY("account") REFERENCES "account"("id");
ALTER TABLE "cell" ADD CONSTRAINT "cell_document_fkey" FOREIGN KEY("document") REFERENCES "document"("id");
ALTER TABLE "cell" ADD CONSTRAINT "cell_code_fkey" FOREIGN KEY("code") REFERENCES "code"("id");
ALTER TABLE "file" ADD CONSTRAINT "file_account_fkey" FOREIGN KEY("account") REFERENCES "account"("id");
ALTER TABLE "revision" ADD CONSTRAINT "revision_account_fkey" FOREIGN KEY("account") REFERENCES "account"("id");
ALTER TABLE "revision" ADD CONSTRAINT "revision_document_fkey" FOREIGN KEY("document") REFERENCES "document"("id");

-- added some updates

ALTER TABLE "account" ALTER COLUMN "subscribed" DROP DEFAULT;
ALTER TABLE "document" ADD CONSTRAINT "document_revision_fkey" FOREIGN KEY("revision") REFERENCES "revision"("id");
ALTER TABLE "code" ADD CONSTRAINT "unique_code" UNIQUE("hash");

-- unique on cells too

ALTER TABLE "cell" ADD CONSTRAINT "unique_cell" UNIQUE("document","code","name","uuid");

|]

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
