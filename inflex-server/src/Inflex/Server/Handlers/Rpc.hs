{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | RPC handlers.

module Inflex.Server.Handlers.Rpc where

import           Criterion.Measurement
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
       revisedDocument <- runDB (getRevisedDocument loginAccountId docId)
       loadRevisedDocument revisedDocument)

--------------------------------------------------------------------------------
-- Refresh document

rpcRefreshDocument :: Shared.RefreshDocument -> Handler Shared.OutputDocument
rpcRefreshDocument Shared.RefreshDocument {documentId, document} =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       RevisedDocument {..} <-
         runDB (getRevisedDocument loginAccountId documentId)
       now <- liftIO getCurrentTime
       start <- liftIO getTime
       mloaded <-
         liftIO
           (timeout
              (1000 * milliseconds)
              (do !x <- (loadInputDocument document)
                  pure x))
       end <- liftIO getTime
       runDB
         (setInputDocument
            now
            loginAccountId
            documentKey
            revisionId
            document)
       case mloaded of
         Just loaded -> do
           glog (DocumentRefreshed (end - start))
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
       RevisedDocument {..} <-
         runDB (getRevisedDocument loginAccountId documentId)
       case update' of
         Shared.CellUpdate Shared.UpdateCell { uuid
                                             , update = Shared.UpdatePath {path}
                                             } -> do
           start <- liftIO getTime
           case applyUpdateToDocument update' (revisionContent revision) of
             Left transformError -> do
               glog UpdateTransformError
               pure
                 (Shared.NestedError
                    (Shared.NestedCellError
                       { Shared.error = transformErrorToCellError transformError
                       , path = path
                       }))
             Right inputDocument -> do
               outputDocument <- liftIO (loadInputDocument inputDocument)
               case cellHadErrorInNestedPlace uuid path outputDocument of
                 Nothing -> do
                   end <- liftIO getTime
                   now <- liftIO getCurrentTime
                   runDB
                     (setInputDocument
                        now
                        loginAccountId
                        documentKey
                        revisionId
                        inputDocument)
                   glog (CellUpdated (end - start))
                   pure (Shared.UpdatedDocument outputDocument)
                 Just cellError -> do
                   glog CellErrorInNestedPlace
                   pure
                     (Shared.NestedError
                        (Shared.NestedCellError
                           {Shared.error = cellError, path = path})))

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

--------------------------------------------------------------------------------
-- Undo/Redo document

rpcUndoDocument :: Shared.DocumentId -> Handler Shared.OutputDocument
rpcUndoDocument docId =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       revisedDocument <-
         runDB
           (do RevisedDocument {..} <- getRevisedDocument loginAccountId docId
               mrevisionId <-
                 selectFirst
                   [RevisionId <. revisionId, RevisionDocument ==. documentKey]
                   [Desc RevisionId]
               case mrevisionId of
                 Just (Entity revisionId' revision') -> do
                   update revisionId [RevisionActive =. False]
                   update revisionId' [RevisionActive =. True]
                   pure
                     RevisedDocument
                       {revisionId = revisionId', revision = revision', ..}
                 Nothing -> pure RevisedDocument {..})
       loadRevisedDocument revisedDocument)

rpcRedoDocument :: Shared.DocumentId -> Handler Shared.OutputDocument
rpcRedoDocument docId =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       revisedDocument <-
         runDB
           (do RevisedDocument {..} <- getRevisedDocument loginAccountId docId
               mrevisionId <-
                 selectFirst
                   [RevisionId >. revisionId, RevisionDocument ==. documentKey]
                   [Asc RevisionId]
               case mrevisionId of
                 Just (Entity revisionId' revision') -> do
                   update revisionId [RevisionActive =. False]
                   update revisionId' [RevisionActive =. True]
                   pure
                     RevisedDocument
                       {revisionId = revisionId', revision = revision', ..}
                 Nothing -> pure RevisedDocument {..})
       loadRevisedDocument revisedDocument)

--------------------------------------------------------------------------------
-- Helpers

data RevisedDocument = RevisedDocument
  { documentKey :: DocumentId
  , revisionId :: RevisionId
  , revision :: Revision
  }

getRevisedDocument :: AccountID -> Shared.DocumentId -> YesodDB App RevisedDocument
getRevisedDocument loginAccountId docId = do
  mdoc <-
    selectFirst
      [ DocumentAccount ==. fromAccountID loginAccountId
      , DocumentId ==. toSqlKey (fromIntegral docId)
      ]
      []
  case mdoc of
    Nothing -> notFound
    Just (Entity documentKey _document) -> do
      mrevision <-
        selectFirst
          [RevisionDocument ==. documentKey, RevisionActive ==. True]
          []
      case mrevision of
        Nothing -> notFound
        Just (Entity revisionId revision) -> do
          pure RevisedDocument {..}

setInputDocument ::
     UTCTime
  -> AccountID
  -> DocumentId
  -> RevisionId
  -> Shared.InputDocument1
  -> YesodDB App ()
setInputDocument now accountId documentId revisionId inputDocument = do
  update revisionId [RevisionActive =. False]
  insert_
    Revision
      { revisionAccount = fromAccountID accountId
      , revisionDocument = documentId
      , revisionCreated = now
      , revisionContent = inputDocument
      , revisionActive = True
      , revisionActivated = now
      }
  revisions <- selectKeysList [RevisionDocument ==. documentId] [Asc RevisionId]
  config <- fmap appConfig getYesod
  case revisions of
    [] -> pure ()
    (earliestRevision:_) ->
      if length revisions > maxRevisionsPerDoc config
        then delete earliestRevision
        else pure ()

--------------------------------------------------------------------------------
-- Loading

loadRevisedDocument :: RevisedDocument -> Handler Shared.OutputDocument
loadRevisedDocument RevisedDocument{..} = do
  start <- liftIO getTime
  mloaded <-
    liftIO
      (timeout
         (1000 * milliseconds)
         (do !x <- loadInputDocument (revisionContent revision)
             pure x))
  end <- liftIO getTime
  case mloaded of
    Just loaded -> do
      glog (DocumentLoaded (end - start))
      pure loaded
    Nothing -> do
      glog TimeoutExceeded
      invalidArgs ["timeout: exceeded " <> T.pack (show milliseconds) <> "ms"]
