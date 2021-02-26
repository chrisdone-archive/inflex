{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Migrations.

module Inflex.Migrate where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Reader
import           Data.Coerce
import           Data.String.Quote
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Time
import           Database.Persist
import           Database.Persist.Postgresql as X
import           Inflex.Server.App
import           Inflex.Server.Types
import           Stripe
import           Yesod

manualMigration :: (MonadIO m, MonadThrow m) => StripeConfig -> x -> ReaderT SqlBackend m ()
manualMigration stripeConfig _x
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
  let loop = do
        version :: Int <-
          fmap
            (foldl (+) 0 . fmap unSingle)
            (rawSql "SELECT version FROM schema_versions;" [])
        liftIO $ putStrLn ("Schema version: " ++ show version)
        case version of
          0 -> schema0 >> loop
          1 -> schema1 >> loop
          2 -> schema2 >> loop
          3 -> schema3 >> loop
          4 -> schema4 >> loop
          5 -> schema5 >> loop
          6 -> schema6 >> loop
          7 -> schema7 stripeConfig >> loop
          8 -> schema8 >> loop
          9 -> liftIO $ putStrLn "OK."
          _ -> error ("At mysterious schema version: " ++ show version)
  loop

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

schema4  = run [s|
INSERT INTO schema_versions VALUES (1);
CREATE TABLE "revision"("id" SERIAL8  PRIMARY KEY UNIQUE,"account" INT8 NOT NULL,"document" INT8 NOT NULL,"created" TIMESTAMP WITH TIME ZONE NOT NULL,"content" VARCHAR NOT NULL,"active" BOOLEAN NOT NULL,"activated" TIMESTAMP WITH TIME ZONE NOT NULL);

INSERT INTO revision (account,document,created,content,active,activated)
  SELECT d.account as account,
         d.id as document,
         d.updated as created,
         d.content,
         true as active,
         now() as activated
  FROM document d;

ALTER TABLE "document" DROP COLUMN "content";

-- For fast lookup of docs by their name.
CREATE INDEX idx_doc_name ON document(name);
-- For fast lookup of revisions by their document_id+activeness.
CREATE INDEX idx_revision_doc_active ON revision (document, active);
 |]

schema5  = run [s|
INSERT INTO schema_versions VALUES (1);
CREATE TABLE "file"("id" SERIAL8  PRIMARY KEY UNIQUE,"account" INT8 NOT NULL,"name" VARCHAR NOT NULL,"created" TIMESTAMP WITH TIME ZONE NOT NULL,"hash" BYTEA NOT NULL,"bytes" INT8 NOT NULL,"mime" VARCHAR NOT NULL);
CREATE INDEX file_account_hash ON file (account,hash);
CREATE INDEX file_account ON file (account);
|]

schema6  = run [s|
INSERT INTO schema_versions VALUES (1);
CREATE INDEX document_account ON document (account);
|]

schema7 _stripeConfig = do
  liftIO $ putStrLn "Migration requires manual intervention."
  --
  -- This code no longer compiles; the migration it performs has now
  -- been added to the type system. So no further action is required.
  --
  -- accounts <- selectList [AccountCustomerId ==. Nothing] []
  -- results <- forM
  --   accounts
  --   (\(Entity accountId Account {accountEmail}) -> do
  --      customerCreateResult <- createCustomer stripeConfig (coerce accountEmail)
  --      case customerCreateResult of
  --        Left err -> do
  --          liftIO $ putStrLn ("Error creating customer:" ++ show err)
  --          pure False
  --        Right CreateCustomerResponse {id = customerId} -> do
  --          update accountId [AccountCustomerId =. Just (coerce customerId)]
  --          now <- liftIO $ getCurrentTime
  --          liftIO $ putStrLn (show now ++ ": OK.")
  --          liftIO $ threadDelay (1000 * 100)
  --          pure True)
  -- if and results || null results
  --    then do
  --         liftIO $ putStrLn (show (length results) ++ " customers created in Stripe.")
  --         run
  --            [s|
  --          INSERT INTO schema_versions VALUES (1);
  --          ALTER TABLE account ALTER customer_id SET NOT NULL;
  --          |]
  --    else liftIO $ putStrLn "Migration failed, keeping existing schema version."

schema8  = run [s|
INSERT INTO schema_versions VALUES (1);
ALTER TABLE account ADD subscribed BOOL NOT NULL DEFAULT FALSE;
|]
