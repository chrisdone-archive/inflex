{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module Inflex.Server.App where

import           Control.Monad.Logger as MonadLogger
import           Control.Monad.Reader
import           Data.Functor.Contravariant
import           Data.Pool
import           Data.Profunctor
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
import           RIO hiding (Handler)
import           RIO.Warp
import           RIO.Yesod
import           Yesod hiding (Html, Field)
import           Yesod.Lucid

data App = App
  { appPool :: !(Pool SqlBackend)
  , appConfig :: !Config
  , appLogFunc :: GLogFunc AppMsg
  }

instance Yesod App where
  makeSessionBackend _  = pure Nothing
  messageLoggerSource App {appLogFunc} =
    yesodLoggerSource (contramap YesodMsg appLogFunc)

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    App {appPool = pool} <- getYesod
    runSqlPool action pool

-- | App log message.
data AppMsg
  = YesodMsg YesodLog
  | AppWaiMsg WaiMsg
  | DatabaseMsg DatabaseLog
  deriving (Show)

-- | A generic log output.
data DatabaseLog =
  DatabaseLog MonadLogger.Loc
              MonadLogger.LogSource
              MonadLogger.LogLevel
              MonadLogger.LogStr
  deriving (Show)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

mkYesodData "App" $(parseRoutesFile "config/routes")

fromAccountId :: AccountId -> AccountID
fromAccountId = AccountID . fromSqlKey

fromAccountID :: AccountID -> AccountId
fromAccountID (AccountID i) = toSqlKey i

type FormValidate = YesodDB App

type Form error a = Forge.Form 'Forge.Unverified FormValidate (Lucid App ()) (Field (Reader (Route App -> Text))) error a
type VerifiedForm error a = Forge.VerifiedForm 'Forge.Unverified FormValidate (Lucid App ()) (Field (Reader (Route App -> Text))) error a
