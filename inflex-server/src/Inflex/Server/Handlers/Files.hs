{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Files
  ( postUploadFileR
  -- , readFileFromHash
  ) where

import qualified Data.ByteString.Lazy as L
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib
import           Data.IORef
import qualified Data.Text as T
import           Data.Time
import           Inflex.Server.App
import           Inflex.Server.App as Model
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.Types.Sha256
import           Path
import           Yesod
import           Yesod.Core.Types as Yesod

-- TODO: file upload limit

postUploadFileR :: Handler ()
postUploadFileR =
  withLogin
    (\_ LoginState {loginAccountId} -> do
       fileInfo <- getFile
       if fileContentType fileInfo == "text/csv"
         then do
           !(hash, bytes, len) <- hashFile fileInfo
           !dir <- fmap (uploadsDir . appConfig) getYesod
           now <- liftIO getCurrentTime
           runDB
             (insert_
                Model.File
                  { fileAccount = fromAccountID loginAccountId
                  , fileName = Yesod.fileName fileInfo
                  , fileCreated = now
                  , fileBytes = fromIntegral len -- TODO: fromIntegral bad
                  , fileHash = hash
                  , fileMime = fileContentType fileInfo
                  , fileContent = bytes
                  })
           redirect AppDashboardR
         else invalidArgs ["invalid file type, should be csv"])

getFile :: Handler FileInfo
getFile = do
  (_headers, files) <- runRequestBody
  case files of
    [(_name, fileInfo)] -> pure fileInfo
    _ ->
      invalidArgs
        [ "expected a single file upload, but got " <>
          T.pack (show (length files))
        ]

hashFile :: MonadIO m => FileInfo -> m (Sha256, ByteString, Int)
hashFile fileInfo =
  liftIO
    (do lenref <- newIORef 0
        let countBytes =
              CL.mapM
                (\chunk ->
                   liftIO (modifyIORef' lenref (+ S.length chunk)) *> pure chunk)
        !compressed <-
          runConduitRes
            (fileSourceRaw fileInfo .| countBytes .| -- gzip .|
             fmap L.toStrict CB.sinkLbs)
        len <- readIORef lenref
        pure (sha256ByteString compressed, compressed, len))

-- readFileFromHash :: Sha256 -> Handler L.ByteString
-- readFileFromHash hash = do
--   dir <- fmap (uploadsDir . appConfig) getYesod
--   casfilename <- parseRelFile (sha256AsHexString hash)
--   let casPath = toFilePath (dir </> casfilename)
--   lbs <- liftIO (runConduitRes (CB.sourceFile casPath .| ungzip .| CB.sinkLbs))
--   -- liftIO (print lbs)
--   pure lbs
