{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

-- | Shared data types.

module Inflex.Schema where

import           Control.Applicative
import           Data.Aeson (FromJSON(..), Options, ToJSON(..), defaultOptions, (.:), withObject, Value)
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Persist.TH
import           GHC.Generics

--------------------------------------------------------------------------------
-- Types

newtype UUID = UUID Text
 deriving (Eq, Ord, FromJSON, ToJSON, Show)

$types

--------------------------------------------------------------------------------
-- Decoding options

opts :: Options
opts = defaultOptions

--------------------------------------------------------------------------------
-- Derivings

deriving instance Generic None
deriving instance Show None
instance ToJSON None
instance FromJSON None

deriving instance Generic Result
deriving instance Show Result
instance ToJSON Result
instance FromJSON Result

deriving instance Generic CellError
deriving instance Show CellError
instance ToJSON CellError
instance FromJSON CellError

deriving instance Generic FillError
deriving instance Show FillError
instance ToJSON FillError
instance FromJSON FillError

deriving instance Generic InputDocument
deriving instance Show InputDocument
instance ToJSON InputDocument
instance FromJSON InputDocument

deriving instance Generic InputDocument1
deriving instance Show InputDocument1
instance ToJSON InputDocument1
instance FromJSON InputDocument1 where
  parseJSON =
    withObject
      "InputDocument1"
      (\o -> do
         cells <- o .: "cells" <|> fmap migrateV1 (o .: "cells")
         pure InputDocument1 {cells})
    where
      migrateV1 :: Vector InputCell -> Vector InputCell1
      migrateV1 =
        V.imap
          (\order InputCell {..} ->
             InputCell1 {version = versionRefl, order, ..})

deriving instance Generic InputCell1
deriving instance Show InputCell1
instance ToJSON InputCell1
instance FromJSON InputCell1

deriving instance Generic RefreshDocument
deriving instance Show RefreshDocument
instance ToJSON RefreshDocument
instance FromJSON RefreshDocument

deriving instance Generic OutputDocument
deriving instance Show OutputDocument
instance ToJSON OutputDocument
instance FromJSON OutputDocument

deriving instance Generic InputCell
deriving instance Show InputCell
instance ToJSON InputCell
instance FromJSON InputCell

deriving instance Generic OutputCell
deriving instance Show OutputCell
instance ToJSON OutputCell
instance FromJSON OutputCell

deriving instance Real DocumentId
deriving instance Enum DocumentId
deriving instance Ord DocumentId
deriving instance Eq DocumentId
deriving instance Num DocumentId
deriving instance Integral DocumentId
deriving instance Generic DocumentId
deriving instance Show DocumentId
instance ToJSON DocumentId
instance FromJSON DocumentId

--------------------------------------------------------------------------------
-- Version infra

parseVersion :: forall v. Version v => Value -> Parser v
parseVersion j = do
  i <- parseJSON j
  if i == versionNumber (versionRefl :: v)
    then pure (versionRefl :: v)
    else fail
           ("Version mismatch, expected: " <> show (versionNumber (versionRefl :: v)) <>
            ", but got: " <>
            show i)

versionToJSON :: forall v. Version v => v -> Value
versionToJSON v = toJSON (versionNumber v)

--------------------------------------------------------------------------------
-- Versions

deriving instance Show Version1
instance Version Version1 where versionNumber _ = 1; versionRefl = Version1
instance FromJSON Version1 where parseJSON = parseVersion
instance ToJSON Version1 where toJSON = versionToJSON

$(derivePersistFieldJSON "InputDocument")
$(derivePersistFieldJSON "OutputDocument")
$(derivePersistFieldJSON "InputCell")
$(derivePersistFieldJSON "OutputCell")

$(derivePersistFieldJSON "InputDocument1")
$(derivePersistFieldJSON "InputCell1")
