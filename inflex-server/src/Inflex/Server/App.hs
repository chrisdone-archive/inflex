{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Inflex.Server.App where

import           Control.Monad.Logger as MonadLogger
import           Control.Monad.Reader
import           Data.Functor.Contravariant
import           Data.Pool
import           Data.Text (Text)
import           Data.Time
import           Database.Persist.Quasi
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Backend
import qualified Inflex.Schema as Shared
import           Inflex.Server.Forge
import           Inflex.Server.Types
import           Inflex.Server.Types.Blog
import           Inflex.Server.Types.Sha256
import qualified Network.Wai.Parse
import           Optics (toLensVL, makePrisms, makeLensesFor, preview)
import           RIO hiding (Handler, preview)
import           RIO.Warp
import           RIO.Yesod
import           Yesod hiding (Html, Field, fileName)
import qualified Yesod.Core.Types as Yesod
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- Types

data App = App
  { appPool :: !(Pool SqlBackend)
  , appConfig :: !Config
  , appLogFunc :: GLogFunc AppMsg
  }

-- | App log message.
data AppMsg
  = YesodMsg YesodLog
  | AppWaiMsg WaiMsg
  | DatabaseMsg DatabaseLog
  | ServerMsg ServerMsg
  deriving (Show)

data ServerMsg
  = DocumentLoaded Double
  | TimeoutExceeded
  | DocumentRefreshed Double
  | UpdateTransformError
  | CellUpdated Double
  | CellErrorInNestedPlace
  | OpenDocument
  | CreateDocument
  | DeleteDocument
  | RenameDocument
  deriving (Show)

-- | A generic log output.
data DatabaseLog =
  DatabaseLog MonadLogger.Loc
              MonadLogger.LogSource
              MonadLogger.LogLevel
              MonadLogger.LogStr
  deriving (Show)

--------------------------------------------------------------------------------
-- TH-based derivings

$(makePrisms ''AppMsg)
$(makeLensesFor [("appLogFunc","appLogFuncLens")] ''App)
$(share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models"))
$(mkYesodData "App" $(parseRoutesFile "config/routes"))

--------------------------------------------------------------------------------
-- Instances

instance Yesod App where
  makeSessionBackend _ = pure Nothing
  maximumContentLength app mroute =
    case mroute of
      Just UploadFileR {} ->
        pure (fromIntegral (maxUploadSizeBytes (appConfig app)))
      _ -> pure (fromIntegral (defaultMaxRequestBytes (appConfig app)))
  messageLoggerSource App {appLogFunc} =
    yesodLoggerSource (contramap YesodMsg appLogFunc)
  fileUpload _ _ = FileUploadDisk Network.Wai.Parse.tempFileBackEnd

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    App {appPool = pool} <- getYesod
    runSqlPool action pool

instance HasGLogFunc (Yesod.HandlerData App App) where
  type GMsg (Yesod.HandlerData App App) = ServerMsg
  gLogFuncL =
    glogFuncLYesod
      (\f ->
         toLensVL
           appLogFuncLens
           (fmap (contramapMaybeGLogFunc (preview _ServerMsg)) .
            f . contramap ServerMsg))

--------------------------------------------------------------------------------
-- Helpers

fromAccountId :: AccountId -> AccountID
fromAccountId = AccountID . fromSqlKey

fromAccountID :: AccountID -> AccountId
fromAccountID (AccountID i) = toSqlKey i

--------------------------------------------------------------------------------
-- Handy aliases

type FormValidate = YesodDB App

type Form error a = Forge.Form 'Forge.Unverified FormValidate (Lucid App ()) (Field (Reader (Route App -> Text))) error a
type VerifiedForm error a = Forge.VerifiedForm 'Forge.Unverified FormValidate (Lucid App ()) (Field (Reader (Route App -> Text))) error a
