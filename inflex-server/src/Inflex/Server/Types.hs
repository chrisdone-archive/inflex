{-# OPTIONS_GHC -fno-warn-orphans #-}
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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Inflex.Server.Types where

import           Control.Monad
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID as UUID
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
import           Optics.TH
import           Stripe
import           Yesod hiding (Html)

data Config = Config
  { stripeConfig :: StripeConfig
  , databaseConn :: Text
  } deriving (Generic)
instance FromJSON Config

-- | TODO: Implement manual PathPiece
newtype DocumentName = DocumentName Text
  deriving (Show, Read, PathPiece, Eq)

-- | TODO: Implement manual PathPiece
newtype Username =
  Username Text
  deriving ( Show
           , Read
           , PathPiece
           , Eq
           , PersistFieldSql
           , PersistField
           , FromJSON -- TODO: Manual parse
           , ToJSON
           )

parseUsername :: Text -> Maybe Username
parseUsername (T.strip -> txt) =
  -- TODO: Add limited hyphens.
  if T.all isAlphaNum txt && T.length txt >= 3 && T.all isAlpha (T.take 1 txt)
    then pure (Username txt)
    else Nothing

newtype Password = Password Text
  deriving (Read, PathPiece, Eq, PersistFieldSql, PersistField, FromJSON, ToJSON)
instance Show Password where
  show _ = "Password _"

parsePassword :: Text -> Maybe Password
parsePassword txt =
  -- TODO: Add more restrictions?
  if T.length txt >= 16
    then pure (Password txt)
    else Nothing

instance PersistFieldSql UUID where
  sqlType _ = SqlString
instance PersistField UUID where
 toPersistValue = toPersistValue . UUID.toText
 fromPersistValue = fromPersistValue >=> maybe (Left "Bad UUID") Right . UUID.fromText

newtype Email =
  Email Text
  deriving (Show, Read, Eq, PersistFieldSql, PersistField, FromJSON, ToJSON)

data SessionState
  = Unregistered RegistrationState
  | Registered
  deriving (Show, Generic)
instance FromJSON SessionState
instance ToJSON SessionState

data RegistrationDetails = RegistrationDetails
  { registerEmail :: !Email
  , registerPassword :: !Password
  , registerUsername :: !Username
  } deriving (Show, Generic)
instance FromJSON RegistrationDetails
instance ToJSON RegistrationDetails

data RegistrationState
  = EnterDetails
  | CreateCheckout RegistrationDetails
  | WaitingForStripe RegistrationDetails
  deriving (Show, Generic)
instance FromJSON RegistrationState
instance ToJSON RegistrationState

newtype SessionUUID = SessionUUID {unSessionUUID :: UUID}
  deriving (Show, PersistField, PersistFieldSql)

$(makePrisms ''RegistrationState)
$(makePrisms ''SessionState)

$(derivePersistFieldJSON "SessionState")
$(derivePersistFieldJSON "RegistrationState")
