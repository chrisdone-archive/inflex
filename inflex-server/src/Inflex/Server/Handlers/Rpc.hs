{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | RPC handlers.

module Inflex.Server.Handlers.Rpc where

import           Data.Foldable
import           Data.Maybe
import           Data.Time
import           Database.Persist.Sql
import qualified Inflex.Schema as Shared
import           Inflex.Server.App
import           Inflex.Server.Compute
import           Inflex.Server.Session
import           Inflex.Server.Transforms
import           Inflex.Server.Types
import           Yesod hiding (Html)

--------------------------------------------------------------------------------
-- Load document

rpcLoadDocument :: Shared.DocumentId -> Handler Shared.OutputDocument
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

rpcRefreshDocument :: Shared.RefreshDocument -> Handler Shared.OutputDocument
rpcRefreshDocument Shared.RefreshDocument {documentId, document} =
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

--------------------------------------------------------------------------------
-- Update document

rpcUpdateDocument :: Shared.UpdateDocument -> Handler Shared.UpdateResult
rpcUpdateDocument Shared.UpdateDocument {documentId, update = update'} =
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
         Just (Entity documentId' document) -> do
           let inputDocument =
                 applyUpdateToDocument update' (documentContent document)
               loadedDocument = loadInputDocument inputDocument
           case update' of
             Shared.CellUpdate Shared.UpdateCell { uuid
                                                 , update = Shared.UpdatePath {path}
                                                 } ->
               case cellHadErrorInNestedPlace uuid path loadedDocument of
                 Nothing -> do
                   now <- liftIO getCurrentTime
                   runDB
                     (update
                        documentId'
                        [ DocumentContent =. inputDocument
                        , DocumentUpdated =. now
                        ])
                   pure (Shared.UpdatedDocument loadedDocument)
                 Just cellError ->
                   pure
                     (Shared.NestedError
                        (Shared.NestedCellError {Shared.error = cellError, path = path})))

-- | Determine whether there was an error in the cell at the place of
-- the update. If so, return it! We can then nicely display it to the
-- user, rather than breaking the cell structure.
cellHadErrorInNestedPlace ::
     Shared.UUID
  -> Shared.DataPath
  -> Shared.OutputDocument
  -> Maybe Shared.CellError
cellHadErrorInNestedPlace uuid0 path (Shared.OutputDocument cells) =
  case path of
    Shared.DataHere -> Nothing
    _ ->
      listToMaybe
        (mapMaybe
           (\Shared.OutputCell {uuid, result} ->
              case result of
                Shared.ResultError cellError | uuid == uuid0 -> pure cellError
                _ -> Nothing)
           (toList cells))
