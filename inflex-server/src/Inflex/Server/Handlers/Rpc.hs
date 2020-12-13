{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | RPC handlers.

module Inflex.Server.Handlers.Rpc where

import           Control.DeepSeq
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Database.Persist.Sql
import qualified Inflex.Schema as Shared
import           Inflex.Server.App
import           Inflex.Server.Compute
import           Inflex.Server.Session
import           Inflex.Server.Transforms
import           Inflex.Server.Types
import           RIO (glog)
import           System.Timeout
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
         Just (Entity _ Document {documentContent = document}) -> do
           mloaded <-
             liftIO
               (timeout
                  (1000 * milliseconds)
                  (do let !x = force (loadInputDocument document)
                      pure x))
           case mloaded of
             Just loaded -> do
               glog DocumentLoaded
               pure loaded
             Nothing -> do
               glog TimeoutExceeded
               invalidArgs
                 ["timeout: exceeded " <> T.pack (show milliseconds) <> "ms"])

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
           mloaded <-
             liftIO
               (timeout
                  (1000 * milliseconds)
                  (do let !x = force (loadInputDocument document)
                      pure x))
           runDB
             (update
                (entityKey documentEntity)
                [DocumentContent =. document, DocumentUpdated =. now])
           case mloaded of
             Just loaded -> do
               glog DocumentRefreshed
               pure loaded
             Nothing -> do
               glog TimeoutExceeded
               invalidArgs
                 ["timeout: exceeded " <> T.pack (show milliseconds) <> "ms"])

milliseconds :: Int
milliseconds = 1000

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
           case update' of
             Shared.CellUpdate Shared.UpdateCell { uuid
                                                 , update = Shared.UpdatePath {path}
                                                 } ->
               case applyUpdateToDocument update' (documentContent document) of
                 Left transformError -> do
                   glog UpdateTransformError
                   pure
                     (Shared.NestedError
                        (Shared.NestedCellError
                           { Shared.error =
                               transformErrorToCellError transformError
                           , path = path
                           }))
                 Right inputDocument ->
                   case cellHadErrorInNestedPlace uuid path outputDocument of
                     Nothing -> do
                       now <- liftIO getCurrentTime
                       runDB
                         (update
                            documentId'
                            [ DocumentContent =. inputDocument
                            , DocumentUpdated =. now
                            ])
                       glog CellUpdated
                       pure (Shared.UpdatedDocument outputDocument)
                     Just cellError -> do
                       glog CellErrorInNestedPlace
                       pure
                         (Shared.NestedError
                            (Shared.NestedCellError
                               {Shared.error = cellError, path = path}))
                   where outputDocument :: Shared.OutputDocument
                         outputDocument = loadInputDocument inputDocument)

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

transformErrorToCellError :: TransformError -> Shared.CellError
transformErrorToCellError =
  \case
    TransformedParseError {} -> Shared.SyntaxError
    OriginalSourceNotParsing {} -> Shared.SyntaxError -- TODO: Make explicit constructor.
