{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

-- | Shared data types.

module Inflex.Schema where

import Data.Aeson (FromJSON, Options, ToJSON, defaultOptions)
import Data.Text (Text)
import Data.Vector (Vector)
import Database.Persist.TH
import GHC.Generics

--------------------------------------------------------------------------------
-- Types

newtype UUID = UUID Text
 deriving (Eq, Ord, FromJSON, ToJSON, Show)

data None =
  None

newtype DocumentId =
  DocumentId Int

data OutputDocument = OutputDocument
  { cells :: Vector OutputCell
  }

data RefreshDocument = RefreshDocument
  { document :: InputDocument
  , documentId :: DocumentId
  }

data InputDocument = InputDocument
  { cells :: Vector InputCell
  }

data OutputCell = OutputCell
  { uuid :: UUID
  , name :: Text
  , code :: Text
  , result :: Result
  }

data InputCell = InputCell
  { uuid :: UUID
  , name :: Text
  , code :: Text
  }

data Result
  = ResultError CellError
  | ResultOk Text

data CellError
  = SyntaxError -- TODO: more info.
  | FillErrors (Vector FillError)
  | CyclicCells (Vector Text)
  | DuplicateCellName
  | CellRenameErrors
  | CellTypeError -- TODO: more info.
  | CellStepEror -- TODO: more info.

data FillError
  = NoSuchGlobal Text
  | OtherCellProblem Text


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

$(derivePersistFieldJSON "InputDocument")
$(derivePersistFieldJSON "OutputDocument")
$(derivePersistFieldJSON "InputCell")
$(derivePersistFieldJSON "OutputCell")
