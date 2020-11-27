{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- |

module Inflex.Backend (module X, withBackendPool,manualMigration) where

import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.String.Quote
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Database.Persist.Postgresql as X
import           Yesod

withBackendPool = withPostgresqlPool

manualMigration :: (MonadIO m) => x -> ReaderT SqlBackend m ()
manualMigration _x = do
  -- Set isolation, and validate it.
  run "BEGIN TRANSACTION; SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;"
  level <- fmap (fmap unSingle) (rawSql "SELECT CURRENT_SETTING('transaction_isolation');" [])
  unless (level == ["serializable" :: Text]) (error "Transaction isolation level is not being set properly!")
  -- Setup schema versions table.
  run "CREATE TABLE IF NOT EXISTS schema_versions (version INT, date TIMESTAMP DEFAULT NOW());"
  version :: Int <- fmap (foldl (+) 0 . fmap unSingle) (rawSql "SELECT version FROM schema_versions;" [])
  -- Dispatch a migrateion.
  case version of
    0 -> schema0
    1 -> schema1
    2 -> schema2
    3 -> schema3
    4 -> liftIO $ putStrLn "At correct schema version."
    _ -> error ("At mysterious schema version: " ++ show version)
  -- Commit transaction NOW.
  run "COMMIT;"
  where

run :: (MonadIO m) => Text -> ReaderT SqlBackend m ()
run x = do
  liftIO $ T.putStrLn x
  void $ rawExecute x []

schema0 = run [s|
INSERT INTO schema_versions VALUES (1);
CREATe TABLE "account"("id" SERIAL8  PRIMARY KEY UNIQUE,"username" VARCHAR NULL,"email" VARCHAR NOT NULL,"password" BYTEA NOT NULL,"salt" VARCHAR NOT NULL,"customer_id" VARCHAR NOT NULL);
CREATe TABLE "session"("id" SERIAL8  PRIMARY KEY UNIQUE,"uuid" VARCHAR NOT NULL,"state" VARCHAR NOT NULL,"nonce" VARCHAR NULL);
CREATe TABLE "document"("id" SERIAL8  PRIMARY KEY UNIQUE,"account" INT8 NOT NULL,"name" VARCHAR NOT NULL,"content" VARCHAR NOT NULL,"created" TIMESTAMP WITH TIME ZONE NOT NULL,"updated" TIMESTAMP WITH TIME ZONE NOT NULL);
ALTER TABLE "document" ADD CONSTRAINT "document_account_fkey" FOREIGN KEY("account") REFERENCES "account"("id");
  |]

schema1 = run [s|
INSERT INTO schema_versions VALUES (1);
CREATE TABLE "early_access_request"("id" SERIAL8  PRIMARY KEY UNIQUE,"created" TIMESTAMP WITH TIME ZONE NOT NULL,"email" VARCHAR NOT NULL,"approved" TIMESTAMP WITH TIME ZONE NULL);
ALTER TABLE "early_access_request" ADD CONSTRAINT "early_access_request_early_access_email" UNIQUE("email");
  |]

schema2 = run [s|
INSERT INTO schema_versions VALUES (1);
ALTER TABLE "session" ADD CONSTRAINT "session_unique_uuid" UNIQUE (uuid);
  |]

schema3 = run [s|
INSERT INTO schema_versions VALUES (1);
ALTER TABLE "account" ALTER COLUMN "customer_id" DROP NOT NULL;
  |]
