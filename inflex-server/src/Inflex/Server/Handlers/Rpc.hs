{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | RPC handlers.

module Inflex.Server.Handlers.Rpc where

import           Data.Time
import           Database.Persist.Sql
import qualified Inflex.Schema as Schema
import           Inflex.Server.App
import           Inflex.Server.Compute
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Yesod hiding (Html)

--------------------------------------------------------------------------------
-- Load document

rpcLoadDocument :: Schema.DocumentId -> Handler Schema.OutputDocument
rpcLoadDocument docId =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       mdoc <-
         runDB
           (selectFirst
              [ DocumentAccount ==. fromAccountID loginAccountId
              , DocumentId ==. toSqlKey (fromIntegral docId)
              ]
              [])
       case mdoc of
         Nothing -> notFound
         Just (Entity _ Document {documentContent = document}) ->
           pure (loadInputDocument document))

--------------------------------------------------------------------------------
-- Refresh document

rpcRefreshDocument :: Schema.RefreshDocument -> Handler Schema.OutputDocument
rpcRefreshDocument Schema.RefreshDocument {documentId, document} =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       mdoc <-
         runDB
           (selectFirst
              [ DocumentAccount ==. fromAccountID loginAccountId
              , DocumentId ==. toSqlKey (fromIntegral documentId)
              ]
              [])
       case mdoc of
         Nothing -> notFound
         Just documentEntity -> do
           now <- liftIO getCurrentTime
           runDB
             (update
                (entityKey documentEntity)
                [DocumentContent =. document, DocumentUpdated =. now])
           pure (loadInputDocument document))
