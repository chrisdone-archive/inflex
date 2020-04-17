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

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
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
newtype Username = Username Text
  deriving (Show, Read, PathPiece, Eq, PersistFieldSql, PersistField)

parseUsername :: Text -> Maybe Username
parseUsername (T.strip -> txt) =
  -- TODO: Add limited hyphens.
  if T.all isAlphaNum txt && T.length txt >= 3 && T.all isAlpha (T.take 1 txt)
    then pure (Username txt)
    else Nothing

newtype Password = Password Text
  deriving (Read, PathPiece, Eq, PersistFieldSql, PersistField)
instance Show Password where
  show _ = "Password _"

parsePassword :: Text -> Maybe Password
parsePassword txt =
  -- TODO: Add more restrictions?
  if T.length txt >= 16
    then pure (Password txt)
    else Nothing

newtype Email = Email Text
  deriving (Show, Read, Eq, PersistFieldSql, PersistField)
