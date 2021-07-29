{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | RPC handlers.

module Inflex.Server.Handlers.Rpc where

import           Criterion.Measurement
import qualified Data.Aeson as Aeson
import           Data.Char
import qualified Data.Csv as Csv
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.UUID as UUID
import           Data.UUID.V4 as UUID
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Database.Esqueleto as E
import           Database.Persist.Sql
import qualified Inflex.Schema as CachedOutputCell (CachedOutputCell(..))
import qualified Inflex.Schema as InputDocument1 (InputDocument1(..))
import qualified Inflex.Schema as Shared
import           Inflex.Server.App
import           Inflex.Server.Compute
import           Inflex.Server.Csv
import           Inflex.Server.Handlers.Files
import           Inflex.Server.Session
import           Inflex.Server.Transforms
import           Inflex.Server.Types
import           Inflex.Server.Types.Sha256
import           Inflex.Types hiding (Cell)
import           Inflex.Types.SHA512
import qualified RIO
import           RIO (glog)
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
-- Update document

rpcUpdateSandbox :: Shared.UpdateSandbox -> Handler Shared.UpdateResult
rpcUpdateSandbox Shared.UpdateSandbox {document, update = update'} =
  applyUpdate
    mempty -- maybe populate later
    update'
    document
    updateDocument
  where
    updateDocument _ = pure ()

rpcUpdateDocument :: Shared.UpdateDocument -> Handler Shared.UpdateResult
rpcUpdateDocument Shared.UpdateDocument {documentId, update = update', seen} =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       RevisedDocument {..} <-
         runDB (getRevisedDocument loginAccountId documentId)
       now <- liftIO getCurrentTime
       applyUpdate
         seen
         update'
         revisedInputDocument
         (runDB . setInputDocument now loginAccountId documentKey revisionId))

applyUpdate ::
     Vector Shared.Hash
  -> Shared.Update
  -> Shared.InputDocument1
  -> (Shared.InputDocument1 -> HandlerFor App ())
  -> HandlerFor App Shared.UpdateResult
applyUpdate seen update' inputDocument0@Shared.InputDocument1 {cells} setInputDoc =
  case update' of
    Shared.CellNew Shared.NewCell {code} -> do
      uuid <- liftIO UUID.nextRandom
      let newcell =
            Shared.InputCell1
              { uuid = Shared.UUID (UUID.toText uuid)
              , name = ""
              , code
              , order = V.length cells + 1
              , version = Shared.versionRefl
              }
          inputDocument =
            inputDocument0 {InputDocument1.cells = cells <> pure newcell}
      outputDocument <-
        forceTimeout TimedLoadDocument (loadInputDocument inputDocument)
      setInputDoc inputDocument
      pure (Shared.UpdatedDocument (cellsToOutputDocument seen outputDocument))
    Shared.CellDelete delete' -> do
      let inputDocument = applyDelete delete' inputDocument0
      outputDocument <-
        forceTimeout TimedLoadDocument (loadInputDocument inputDocument)
      setInputDoc inputDocument
      pure (Shared.UpdatedDocument (cellsToOutputDocument seen outputDocument))
    Shared.CellRename rename -> do
      let inputDocument = applyRename rename inputDocument0
      outputDocument <-
        forceTimeout TimedLoadDocument (loadInputDocument inputDocument)
      setInputDoc inputDocument
      pure (Shared.UpdatedDocument (cellsToOutputDocument seen outputDocument))
    Shared.CellUpdate update''@Shared.UpdateCell { uuid
                                                 , update = Shared.UpdatePath {path}
                                                 } -> do
      case applyUpdateToDocument update'' inputDocument0 of
        Left transformError -> do
          glog UpdateTransformError
          pure
            (Shared.NestedError
               (Shared.NestedCellError
                  { Shared.error = transformErrorToCellError transformError
                  , path = path
                  }))
        Right inputDocument -> do
          outputDocument <-
            forceTimeout TimedLoadDocument (loadInputDocument inputDocument)
          case cellHadErrorInNestedPlace uuid path outputDocument of
            Nothing -> do
              setInputDoc inputDocument
              pure
                (Shared.UpdatedDocument (cellsToOutputDocument seen outputDocument))
            Just cellError -> do
              glog CellErrorInNestedPlace
              pure
                (Shared.NestedError
                   (Shared.NestedCellError
                      {Shared.error = cellError, path = path}))

-- | Determine whether there was an error in the cell at the place of
-- the update. If so, return it! We can then nicely display it to the
-- user, rather than breaking the cell structure.
cellHadErrorInNestedPlace ::
     Shared.UUID
  -> Shared.DataPath
  -> Vector OutputCell
  -> Maybe Shared.CellError
cellHadErrorInNestedPlace uuid0 path cells =
  case path of
    Shared.DataHere -> Nothing
    _ ->
      listToMaybe
        (mapMaybe
           (\OutputCell {uuid, result} ->
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

-- TODO: fix undo/redo commented out code


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
                   -- update revisionId [RevisionActive =. False]
                   -- update revisionId' [RevisionActive =. True]
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
                   -- update revisionId [RevisionActive =. False]
                   -- update revisionId' [RevisionActive =. True]
                   pure
                     RevisedDocument
                       {revisionId = revisionId', revision = revision', ..}
                 Nothing -> pure RevisedDocument {..})
       loadRevisedDocument revisedDocument)

--------------------------------------------------------------------------------
-- Helpers

data RevisedDocument = RevisedDocument
  { documentKey :: !DocumentId
  , revisionId :: !RevisionId
  , revision :: !Revision
  , revisedInputDocument :: !Shared.InputDocument1
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
    Just (Entity documentKey Document {documentRevision = Just revisionId}) -> do
      revision <- get404 revisionId
      cells <-
        fmap
          (fmap
             (\(Entity _ Code {..}, Entity _ Cell {..}, Entity _ RevisionCell {..}) ->
                Shared.InputCell1
                  { uuid = cellUuid
                  , name = cellName
                  , code = codeSource
                  , order = revisionCellOrder
                  , version = Shared.versionRefl
                  }))
          (E.select
             (E.from
                (\row@(code, cell, revisionCell) -> do
                   pure ()
                   -- Join code to cell:
                   E.where_ (code E.^. CodeId E.==. cell E.^. CellCode)
                   -- Join cell to revision cell:
                   E.where_
                     (cell E.^. CellId E.==. revisionCell E.^. RevisionCellCell)
                   -- Join revision cell to revision.
                   E.where_
                     (revisionCell E.^. RevisionCellRevision E.==.
                      E.val revisionId)
                   pure row)))
      let revisedInputDocument =
            Shared.InputDocument1 {cells = V.fromList cells}
      pure RevisedDocument {..}
    _ -> notFound -- Cheeky.

setInputDocument ::
     UTCTime
  -> AccountID
  -> DocumentId
  -> RevisionId
  -> Shared.InputDocument1
  -> YesodDB App ()
setInputDocument now accountId documentId _oldRevisionId inputDocument = do
  revisionId <-
    insert
      Revision
        { revisionAccount = fromAccountID accountId
        , revisionDocument = documentId
        , revisionCreated = now
        }
  for_
    (InputDocument1.cells inputDocument)
    (\Shared.InputCell1 {uuid, name, code, order} -> do
       Entity {entityKey = codeId} <-
         upsert -- TODO: Avoid re-inserting the same code.
           Code
             {codeSource = code, codeHash = sha512Text code, codeCreated = now}
           []
       Entity {entityKey = cellId} <-
         upsert -- TODO: Avoid re-inserting the same cell.
           Cell
             { cellAccount = fromAccountID accountId
             , cellDocument = documentId
             , cellCode = codeId
             , cellCreated = now
             , cellName = name
             , cellUuid = uuid
             }
           []
       insert_
         RevisionCell
           { revisionCellRevision = revisionId
           , revisionCellCell = cellId
           , revisionCellOrder = order
           }
       pure ())
  update documentId [DocumentRevision =. pure revisionId]
  -- TODO: Garbage collect old revisions.
  -- revisions <- selectKeysList [RevisionDocument ==. documentId] [Asc RevisionId]
  -- config <- fmap appConfig getYesod
  -- case revisions of
  --   [] -> pure ()
  --   (earliestRevision:_) ->
  --     if length revisions > maxRevisionsPerDoc config
  --       then delete earliestRevision
  --       else pure ()

--------------------------------------------------------------------------------
-- Loading

loadRevisedDocument :: RevisedDocument -> Handler Shared.OutputDocument
loadRevisedDocument RevisedDocument {..} =
  fmap
    (cellsToOutputDocument mempty) -- TODO: populate the mempty?
    (forceTimeout TimedLoadDocument (loadInputDocument revisedInputDocument))

--------------------------------------------------------------------------------
-- Importing CSV

rpcGetFiles :: Shared.FileQuery -> Handler Shared.FilesOutput
rpcGetFiles (Shared.FileQuery{search = _}) =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       files <-
         runDB
           (selectList
              [FileAccount ==. fromAccountID loginAccountId]
              [Desc FileCreated])
       pure
         (Shared.FilesOutput
            (V.fromList
               (map
                  (\(Entity fileid File {..}) ->
                     Shared.File
                       {name = fileName, id = fromIntegral (fromSqlKey fileid)})
                  files))))

rpcCsvGuessSchema :: Shared.File -> Handler Shared.CsvGuess
rpcCsvGuessSchema file@Shared.File {id = fileId} = do
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       mfile <-
         runDB
           (selectFirst
              [ FileAccount ==. fromAccountID loginAccountId
              , FileId ==. toSqlKey (fromIntegral fileId)
              ]
              [Desc FileCreated])
       case mfile of
         Nothing -> notFound
         Just (Entity _ File {fileHash}) -> do
           bytes <- readFileFromHash fileHash
           pure (case guessCsvSchema file bytes of
                   Left err -> Shared.GuessCassavaFailure err
                   Right (schema, _rows) -> Shared.CsvGuessed schema))

rpcCsvCheckSchema :: Shared.CsvImportSpec -> Handler Shared.CsvCheckStatus
rpcCsvCheckSchema Shared.CsvImportSpec { file = Shared.File {id = fileId}
                                       , ..
                                       } =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       mfile <-
         runDB
           (selectFirst
              [ FileAccount ==. fromAccountID loginAccountId
              , FileId ==. toSqlKey (fromIntegral fileId)
              ]
              [Desc FileCreated])
       case mfile of
         Nothing -> notFound
         Just (Entity _ File {fileHash}) -> do
           bytes <- readFileFromHash fileHash
           case Csv.decodeByName bytes of
             Left _err -> pure (Shared.CsvColumnFailures mempty)
             Right (_headers, _rows :: Vector (HashMap Text Text)) ->
               pure Shared.CsvParsesHappily)

rpcCsvImport :: Shared.CsvImportFinal -> Handler Shared.OutputDocument
rpcCsvImport Shared.CsvImportFinal { csvImportSpec = csvImportSpec@Shared.CsvImportSpec {file = file@Shared.File {id = fileId}}
                                   , documentId
                                   , ..
                                   } =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       mfile <-
         runDB
           (selectFirst
              [ FileAccount ==. fromAccountID loginAccountId
              , FileId ==. toSqlKey (fromIntegral fileId)
              ]
              [Desc FileCreated])
       case mfile of
         Nothing -> notFound
         Just (Entity _ File {fileHash}) -> do
           bytes <- readFileFromHash fileHash
           case Csv.decodeByName bytes of
             Left err ->
               error
                 ("Unexpected CSV parse fail; the schema should have \
                 \been validated, so this is a bug." <>
                  show err)
             Right (headers, rows0 :: Vector (HashMap Text Text)) ->
               case importViaSchema
                      file
                      csvImportSpec
                      (fmap (hashMapToOMap (fmap T.decodeUtf8 headers)) rows0) of
                 Left err ->
                   error
                     ("Unexpected CSV parse fail; the schema should have \
                     \been validated, so this is a bug: " <>
                      show err)
                 Right rows -> do
                   RevisedDocument {..} <-
                     runDB (getRevisedDocument loginAccountId documentId)
                   document <-
                     liftIO
                       (insertImportedCsv
                          csvImportSpec
                          file
                          rows
                          revisedInputDocument)
                   loaded <-
                     forceTimeout TimedLoadDocument (loadInputDocument document)
                   now <- liftIO getCurrentTime
                   runDB
                     (setInputDocument
                        now
                        loginAccountId
                        documentKey
                        revisionId
                        document)
                   pure
                     (cellsToOutputDocument
                        mempty -- TODO: populate cache?
                        loaded))

insertImportedCsv ::
     Shared.CsvImportSpec
  -> Shared.File
  -> Vector (InsOrdHashMap Text (Expression Parsed))
  -> Shared.InputDocument1
  -> IO Shared.InputDocument1
insertImportedCsv csvImportSpec Shared.File {name, id = fileId} rows Shared.InputDocument1 {..} = do
  uuid <- liftIO UUID.nextRandom
  pure
    Shared.InputDocument1
      { cells =
          cells <>
          pure
            (Shared.InputCell1
               { uuid = Shared.UUID (UUID.toText uuid)
                 -- TODO: We can include many more chars here.
               , name = T.filter okChar name <> T.pack (show (fileId :: Int))
               , code = RIO.textDisplay (rowsToArray csvImportSpec rows)
               , order = V.length cells + 1
               , version = Shared.versionRefl
               })
      }
  where
    okChar c = isAlphaNum c || c == '_'

--------------------------------------------------------------------------------
-- Timeouts

forceTimeout :: Timed -> Handler (Maybe a) -> Handler a
forceTimeout timed' m = do
  start <- fmap realToFrac (liftIO getTime)
  v' <- m
  end <- fmap realToFrac (liftIO getTime)
  case v' of
    Nothing -> do
      glog (TimeoutExceeded timed')
      invalidArgs ["timeout: exceeded " <> T.pack (show milliseconds) <> "ms"]
    Just v'' -> do
      glog (Timed timed' (end - start))
      pure v''

--------------------------------------------------------------------------------
-- Producing a cached output document

cellsToOutputDocument :: Vector Shared.Hash -> Vector OutputCell -> Shared.OutputDocument
cellsToOutputDocument seen = Shared.OutputDocument . fmap cons
  where
    cons OutputCell {..} =
      Shared.CachedOutputCell
        { result =
            let hash =
                  Shared.Hash
                    (sha256AsHexText
                       (sha256LazyByteString (Aeson.encode result)))
             in if V.elem hash seen
                  then Shared.CachedResult hash
                  else Shared.FreshResult result hash
        , code =
            let hash =
                  Shared.Hash
                    (sha256AsHexText (sha256LazyByteString (Aeson.encode code)))
             in if V.elem hash seen
                  then Shared.CachedText hash
                  else Shared.FreshText code hash
        , ..
        }
