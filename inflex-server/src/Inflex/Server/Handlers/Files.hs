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
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Zlib
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

postUploadFileR :: Handler ()
postUploadFileR =
  withLogin
    (\_ LoginState {loginAccountId} -> do
       fileInfo <- getFile
       if fileContentType fileInfo == "text/csv"
         then do
           !(hash, bytes) <- hashFile fileInfo
           !casfilename <- parseRelFile (sha256AsHexString hash)
           !dir <- fmap (uploadsDir . appConfig) getYesod
           let casPath = toFilePath (dir </> casfilename)
           liftIO (S.writeFile casPath bytes)
           now <- liftIO getCurrentTime
           runDB
             (insert_
                Model.File
                  { fileAccount = fromAccountID loginAccountId
                  , fileName = Yesod.fileName fileInfo
                  , fileCreated = now
                  , fileBytes = fromIntegral (S.length bytes) -- TODO: Make this conversion explicit.
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

hashFile :: MonadIO m => FileInfo -> m (Sha256, ByteString)
hashFile fileInfo =
  liftIO
    (do !compressed <-
          runConduitRes
            (fileSourceRaw fileInfo .| gzip .| fmap L.toStrict CB.sinkLbs)
        pure (sha256ByteString compressed, compressed))
