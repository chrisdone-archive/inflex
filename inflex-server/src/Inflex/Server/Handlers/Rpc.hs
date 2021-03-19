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
import           Data.Aeson
import           Data.Char
import qualified Data.Csv as Csv
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Time
import           Data.UUID as UUID
import           Data.UUID.V4 as UUID
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Persist.Sql
import qualified Inflex.Schema as InputDocument1 (InputDocument1(..))
import qualified Inflex.Schema as Shared
import           Inflex.Server.App
import           Inflex.Server.Compute
import           Inflex.Server.Csv
import           Inflex.Server.Handlers.Files
import           Inflex.Server.Session
import           Inflex.Server.Transforms
import           Inflex.Server.Types
import           Inflex.Types
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
  applyUpdate update' document updateDocument
  where updateDocument _ = pure ()

rpcUpdateDocument :: Shared.UpdateDocument -> Handler Shared.UpdateResult
rpcUpdateDocument Shared.UpdateDocument {documentId, update = update'} =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       RevisedDocument {..} <-
         runDB (getRevisedDocument loginAccountId documentId)
       now <- liftIO getCurrentTime
       applyUpdate
         update'
         (revisionContent revision)
         (runDB . setInputDocument now loginAccountId documentKey revisionId))

applyUpdate ::
     Shared.Update
  -> Shared.InputDocument1
  -> (Shared.InputDocument1 -> HandlerFor App ())
  -> HandlerFor App Shared.UpdateResult
applyUpdate update' inputDocument0@Shared.InputDocument1 {cells} setInputDoc =
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
      outputDocument <- forceTimeout (loadInputDocument inputDocument)
      setInputDoc inputDocument
      pure (Shared.UpdatedDocument outputDocument)
    Shared.CellDelete delete' -> do
      start <- liftIO getTime
      let inputDocument = applyDelete delete' inputDocument0
      outputDocument <- forceTimeout (loadInputDocument inputDocument)
      end <- liftIO getTime
      setInputDoc inputDocument
      glog (CellUpdated (end - start))
      pure (Shared.UpdatedDocument outputDocument)
    Shared.CellRename rename -> do
      start <- liftIO getTime
      let inputDocument = applyRename rename inputDocument0
      outputDocument <- forceTimeout (loadInputDocument inputDocument)
      end <- liftIO getTime
      setInputDoc inputDocument
      glog (CellUpdated (end - start))
      pure (Shared.UpdatedDocument outputDocument)
    Shared.CellUpdate update''@Shared.UpdateCell { uuid
                                                 , update = Shared.UpdatePath {path}
                                                 } -> do
      start <- liftIO getTime
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
          outputDocument <- forceTimeout (loadInputDocument inputDocument)
          case cellHadErrorInNestedPlace uuid path outputDocument of
            Nothing -> do
              end <- liftIO getTime
              setInputDoc inputDocument
              glog (CellUpdated (end - start))
              pure (Shared.UpdatedDocument outputDocument)
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
loadRevisedDocument RevisedDocument {..} =
  forceTimeout (loadInputDocument (revisionContent revision))

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
           pure (guessCsvSchema file bytes))

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
             Right (_headers, rows0 :: Vector (HashMap Text Text)) ->
               case importViaSchema file csvImportSpec rows0 of
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
                       (insertImportedCsv file rows (revisionContent revision))
                   now <- liftIO getCurrentTime
                   start <- liftIO getTime
                   loaded <- forceTimeout (loadInputDocument document)
                   end <- liftIO getTime
                   runDB
                     (setInputDocument
                        now
                        loginAccountId
                        documentKey
                        revisionId
                        document)
                   glog (DocumentRefreshed (end - start))
                   pure loaded)

insertImportedCsv ::
     Shared.File
  -> Vector (HashMap Text (Expression Parsed))
  -> Shared.InputDocument1
  -> IO Shared.InputDocument1
insertImportedCsv Shared.File {name, id = fileId} rows Shared.InputDocument1 {..} = do
  uuid <- liftIO UUID.nextRandom
  pure
    Shared.InputDocument1
      { cells =
          cells <>
          pure
            (Shared.InputCell1
               { uuid = Shared.UUID (UUID.toText uuid)
               , name = T.filter okChar name <> T.pack (show (fileId :: Int))
               , code = toArray rows
               , order = V.length cells + 1
               , version = Shared.versionRefl
               })
      }
  where
    okChar c = isAlphaNum c || c == '_'
    toArray =
      LT.toStrict . LT.toLazyText . brackets . commas . map toObject . V.toList
    toObject :: HashMap Text (Expression Parsed) -> LT.Builder
    toObject hash = "{" <> fields hash <> "}"
      where
        fields =
          commas .
          map
            (\(key, val) ->
               let asString v =
                     LT.fromLazyText (LT.decodeUtf8 (encode (v :: Text)))
                in asString key <> ":" <> LT.fromText (RIO.textDisplay val)) .
          HM.toList
    brackets :: LT.Builder -> LT.Builder
    brackets x = "[" <> x <> "]"
    commas :: [LT.Builder] -> LT.Builder
    commas = mconcat . List.intersperse ","

--------------------------------------------------------------------------------
-- Timeouts

forceTimeout :: IO (Maybe a) -> Handler a
forceTimeout m = do
  v' <- liftIO m
  case v' of
    Nothing -> do
      glog TimeoutExceeded
      invalidArgs ["timeout: exceeded " <> T.pack (show milliseconds) <> "ms"]
    Just v'' -> pure v''
