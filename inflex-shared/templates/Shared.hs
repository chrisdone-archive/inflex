{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

-- | Shared data types.

module Inflex.Shared where

import Data.Aeson
import Data.Text (Text)
import Data.Vector (Vector)
import Database.Persist.TH
import GHC.Generics

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

deriving instance Generic Command
deriving instance Show Command
instance ToJSON Command
instance FromJSON Command

deriving instance Generic Document
deriving instance Show Document
instance ToJSON Document
instance FromJSON Document

deriving instance Generic Cell
deriving instance Show Cell
instance ToJSON Cell
instance FromJSON Cell

deriving instance Generic DocumentId
deriving instance Show DocumentId
instance ToJSON DocumentId
instance FromJSON DocumentId

$(derivePersistFieldJSON "Document")
