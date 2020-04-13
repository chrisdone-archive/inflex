{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Strict #-}
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

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Pool
import           Data.Text (Text)
import           Database.Persist.Quasi
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Backend
import           Inflex.Server.Forge
import           Inflex.Server.Types
import           Yesod hiding (Html, Field)
import           Yesod.Lucid

data App = App
  { appPool :: !(Pool SqlBackend)
  }

instance Yesod App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

mkYesodData "App" $(parseRoutesFile "config/routes")

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

type Form error a = Forge.Form 'Forge.Unverified Identity (Lucid App ()) (Field (Reader (Route App -> Text))) error a
type VerifiedForm error a = Forge.VerifiedForm 'Forge.Unverified Identity (Lucid App ()) (Field (Reader (Route App -> Text))) error a
