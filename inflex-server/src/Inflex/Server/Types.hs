{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID as UUID
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
import           Inflex.Server.Types.Sha256
import           Lucid
import           Optics.TH
import           Stripe
import           Yesod hiding (Html)

data Config = Config
  { stripeConfig :: StripeConfig
  , databaseConn :: Text
  } deriving (Generic)
instance FromJSON Config

-- | TODO: Implement manual PathPiece
-- | TODO: Stricter FromJSON
newtype DocumentSlug =
  DocumentSlug Text
  deriving ( Show
           , Read
           , PathPiece
           , Eq
           , ToJSON
           , FromJSON
           , PersistFieldSql
           , PersistField
           , ToHtml
           )

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
           , ToHtml
           )

parseUsername :: Text -> Maybe Username
parseUsername (T.strip -> txt) =
  -- TODO: Add limited hyphens.
  if T.all isAlphaNum txt && T.length txt >= 3 && T.all isAlpha (T.take 1 txt)
    then pure (Username txt)
    else Nothing

newtype Password = Password {unPassword :: Text}
  deriving (Read, PathPiece, Eq, PersistFieldSql, PersistField, FromJSON, ToJSON)
instance Show Password where
  show _ = "Password _"

parsePassword :: Text -> Maybe Password
parsePassword txt =
  -- TODO: Add more restrictions?
  if T.length txt >= 16
    then pure (Password (txt))
    else Nothing

sha256Password :: Salt -> Password -> Sha256
sha256Password (Salt s) (Password t) = sha256Text (s <> t)

newtype Salt =
  Salt Text
  deriving (Eq, Ord, PersistFieldSql, PersistField, Show)

instance PersistFieldSql UUID where
  sqlType _ = SqlString
instance PersistField UUID where
 toPersistValue = toPersistValue . UUID.toText
 fromPersistValue = fromPersistValue >=> maybe (Left "Bad UUID") Right . UUID.fromText

newtype Email = Email
  { unEmail :: Text
  } deriving ( Show
             , Read
             , Eq
             , PersistFieldSql
             , PersistField
             , FromJSON
             , ToJSON
             , ToHtml
             )

newtype SessionUUID = SessionUUID {unSessionUUID :: UUID}
  deriving (Show, PersistField, PersistFieldSql)

newtype NonceUUID = NonceUUID {unNonceUUID :: UUID}
  deriving (Show, PersistField, PersistFieldSql)

newtype CustomerId = CustomerId {unCustomerId :: Text}
  deriving (Show, PersistField, PersistFieldSql, FromJSON)

data DecIn = DecIn
  { name :: Text
  , rhs :: Text
  } deriving (Show)
instance ToJSON DecIn where
  toJSON DecIn{name,rhs} = object ["name".=name,"rhs".=rhs]
instance FromJSON DecIn where
  parseJSON j = do
    o <- parseJSON j
    DecIn <$> o .: "name" <*> o .: "rhs"

data SessionState
  = Unregistered RegistrationState
  | Registered LoginState
  | NoSessionState
  deriving (Show, Generic)
instance FromJSON SessionState
instance ToJSON SessionState

data LoginState = LoginState
  { loginEmail :: Email
  , loginUsername :: Maybe Username
  , loginAccountId :: AccountID
  }deriving (Show, Generic)
instance FromJSON LoginState
instance ToJSON LoginState

newtype AccountID = AccountID Int64
  deriving (Show, Generic)
instance FromJSON AccountID
instance ToJSON AccountID

data RegistrationDetails = RegistrationDetails
  { registerEmail :: !Email
  , registerPassword :: !Password
  } deriving (Show, Generic)
instance FromJSON RegistrationDetails
instance ToJSON RegistrationDetails

data RegistrationState
  = EnterDetails (Maybe RegistrationDetails)
  | CreateCheckout RegistrationDetails
  | WaitingForStripe RegistrationDetails
  deriving (Show, Generic)
instance FromJSON RegistrationState
instance ToJSON RegistrationState

$(makePrisms ''RegistrationState)
$(makePrisms ''SessionState)

$(derivePersistFieldJSON "SessionState")
$(derivePersistFieldJSON "RegistrationState")
