{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Shared data types.

module Inflex.Shared where

import Data.Aeson
import GHC.Generics

--------------------------------------------------------------------------------
-- Types

$types

--------------------------------------------------------------------------------
-- Decoding options

opts :: Options
opts = defaultOptions

--------------------------------------------------------------------------------
-- Derivings

deriving instance Generic MyRecord
deriving instance Show MyRecord
instance ToJSON MyRecord
instance FromJSON MyRecord
