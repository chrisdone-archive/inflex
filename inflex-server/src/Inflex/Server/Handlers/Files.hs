{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Files
  ( postUploadFileR
  ) where

import           Control.Monad.Reader
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import           Data.Time
import           Inflex.Server.App
import           Inflex.Server.App as Model
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.Types.Sha256
import           Path
import           System.Directory
import           Yesod
import           Yesod.Core.Types as Yesod

postUploadFileR :: Handler ()
postUploadFileR =
  withLogin
    (\_ LoginState {loginAccountId} -> do
       fileInfo <- getFile
       if fileContentType fileInfo == "text/csv"
         then do
           !hash <- hashFile fileInfo
           !casfilename <- parseRelFile (sha256AsHexString hash)
           !dir <- fmap (uploadsDir . appConfig) getYesod
           let casPath = toFilePath (dir </> casfilename)
           liftIO $ fileMove fileInfo casPath
           now <- liftIO getCurrentTime
           bytes <- liftIO (getFileSize casPath)
           runDB
             (insert_
                Model.File
                  { fileAccount = fromAccountID loginAccountId
                  , fileName = Yesod.fileName fileInfo
                  , fileCreated = now
                  , fileBytes = fromIntegral bytes -- TODO: Make this conversion explicit.
                  , fileHash = hash
                  , fileMime = fileContentType fileInfo
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

hashFile :: MonadIO m => FileInfo -> m Sha256
hashFile fileInfo =
  liftIO
    (do !sha <-
          runConduitRes
            (fileSourceRaw fileInfo .| fmap sha256LazyByteString CB.sinkLbs)
        pure sha)
