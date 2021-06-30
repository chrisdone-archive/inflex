{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

-- | Shared data types.

module Inflex.Schema where

import Frisson
import           Control.Applicative
import           Control.DeepSeq
import           Data.Aeson (FromJSON(..), Options, ToJSON(..), defaultOptions, (.:), withObject, Value)
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Persist.TH
import           GHC.Generics
import           Prelude hiding (Maybe)

--------------------------------------------------------------------------------
-- Types

newtype UUID = UUID Text
 deriving (Eq, Ord, FromJSON, Show, NFData)

newtype Hash = Hash Text
 deriving (Eq, Ord, FromJSON, Show, NFData)

$types

--------------------------------------------------------------------------------
-- Decoding options

opts :: Options
opts = defaultOptions

--------------------------------------------------------------------------------
-- Derivings

instance NFData UpdateSandbox
deriving instance Generic UpdateSandbox
deriving instance Show UpdateSandbox
-- instance ToJSON UpdateSandbox
instance FromJSON UpdateSandbox

instance NFData Optionality
deriving instance Generic Optionality
deriving instance Eq Optionality
deriving instance Show Optionality
-- instance ToJSON Optionality
instance FromJSON Optionality

instance NFData FileQuery
deriving instance Generic FileQuery
deriving instance Show FileQuery
-- instance ToJSON FileQuery
instance FromJSON FileQuery

instance NFData File
deriving instance Generic File
deriving instance Eq File
deriving instance Show File
-- instance ToJSON File
instance FromJSON File

instance NFData CsvImportFinal
deriving instance Generic CsvImportFinal
deriving instance Eq CsvImportFinal
deriving instance Show CsvImportFinal
-- instance ToJSON CsvImportFinal
instance FromJSON CsvImportFinal

instance NFData CsvGuess
deriving instance Eq CsvGuess
deriving instance Generic CsvGuess
deriving instance Show CsvGuess
-- instance ToJSON CsvGuess
instance FromJSON CsvGuess

instance NFData FilesOutput
deriving instance Generic FilesOutput
deriving instance Eq FilesOutput
deriving instance Show FilesOutput
-- instance ToJSON FilesOutput
instance FromJSON FilesOutput

instance NFData CsvImportSpec
deriving instance Generic CsvImportSpec
deriving instance Eq CsvImportSpec
deriving instance Show CsvImportSpec
-- instance ToJSON CsvImportSpec
instance FromJSON CsvImportSpec

instance NFData CsvCheckStatus
deriving instance Generic CsvCheckStatus
deriving instance Eq CsvCheckStatus
deriving instance Show CsvCheckStatus
-- instance ToJSON CsvCheckStatus
instance FromJSON CsvCheckStatus

instance NFData CsvColumnProblem
deriving instance Generic CsvColumnProblem
deriving instance Eq CsvColumnProblem
deriving instance Show CsvColumnProblem
-- instance ToJSON CsvColumnProblem
instance FromJSON CsvColumnProblem

instance NFData CsvColumn
deriving instance Generic CsvColumn
deriving instance Eq CsvColumn
deriving instance Show CsvColumn
-- instance ToJSON CsvColumn
instance FromJSON CsvColumn

instance NFData ColumnAction
deriving instance Generic ColumnAction
deriving instance Eq ColumnAction
deriving instance Show ColumnAction
-- instance ToJSON ColumnAction
instance FromJSON ColumnAction

instance NFData ImportColumn
deriving instance Generic ImportColumn
deriving instance Eq ImportColumn
deriving instance Show ImportColumn
-- instance ToJSON ImportColumn
instance FromJSON ImportColumn

instance NFData CsvColumnType
deriving instance Generic CsvColumnType
deriving instance Eq CsvColumnType
deriving instance Show CsvColumnType
-- instance ToJSON CsvColumnType
instance FromJSON CsvColumnType

instance NFData Result
deriving instance Generic Result
deriving instance Show Result
-- instance ToJSON Result
instance FromJSON Result

instance NFData Tree1
deriving instance Generic Tree1
deriving instance Show Tree1
-- instance ToJSON Tree1
instance FromJSON Tree1

instance NFData Tree2
deriving instance Generic Tree2
deriving instance Show Tree2
-- instance ToJSON Tree2
instance FromJSON Tree2

instance NFData ResultTree
deriving instance Generic ResultTree
deriving instance Show ResultTree
-- deriving instance ToJSON ResultTree
instance FromJSON ResultTree where
  parseJSON j =
    fmap
      ResultTree
      (parseJSON j <|> fmap migrateV2 (parseJSON j) <|>
       fmap (migrateV2 . migrateV1) (parseJSON j))
    where
      migrateV1 :: Text -> Tree1
      migrateV1 text = MiscTree versionRefl text
      migrateV2 :: Tree1 -> Tree2
      migrateV2 =
        \case
          ArrayTree _ trees ->
            ArrayTree2 versionRefl NoOriginalSource (fmap migrateV2 trees)
          RecordTree _ fields ->
            RecordTree2
              versionRefl
              NoOriginalSource
              (fmap migrateV2Field fields)
            where migrateV2Field Field1 {..} =
                    Field2 {version = versionRefl, value = migrateV2 value, ..}
          MiscTree _ text -> MiscTree2 versionRefl NoOriginalSource text

instance NFData CellError
deriving instance Generic CellError
deriving instance Show CellError
-- instance ToJSON CellError
instance FromJSON CellError

instance NFData OriginalSource
deriving instance Generic OriginalSource
deriving instance Show OriginalSource
-- instance ToJSON OriginalSource
instance FromJSON OriginalSource

instance NFData Field1
deriving instance Generic Field1
deriving instance Show Field1
-- instance ToJSON Field1
instance FromJSON Field1

instance NFData Field2
deriving instance Generic Field2
deriving instance Show Field2
-- instance ToJSON Field2
instance FromJSON Field2

instance NFData Row
deriving instance Generic Row
deriving instance Show Row
-- instance ToJSON Row
instance FromJSON Row

instance NFData FillError
deriving instance Generic FillError
deriving instance Show FillError
-- instance ToJSON FillError
instance FromJSON FillError

instance NFData InputDocument
deriving instance Generic InputDocument
deriving instance Show InputDocument
-- instance ToJSON InputDocument
instance FromJSON InputDocument

instance NFData InputDocument1
deriving instance Generic InputDocument1
deriving instance Show InputDocument1
-- instance ToJSON InputDocument1
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

instance NFData InputCell1
deriving instance Generic InputCell1
deriving instance Show InputCell1
-- instance ToJSON InputCell1
instance FromJSON InputCell1

instance NFData RefreshDocument
deriving instance Generic RefreshDocument
deriving instance Show RefreshDocument
-- instance ToJSON RefreshDocument
instance FromJSON RefreshDocument

instance NFData OutputDocument
deriving instance Generic OutputDocument
deriving instance Show OutputDocument
-- instance ToJSON OutputDocument
instance FromJSON OutputDocument

instance NFData InputCell
deriving instance Generic InputCell
deriving instance Show InputCell
-- instance ToJSON InputCell
instance FromJSON InputCell

instance NFData OutputCell
deriving instance Generic OutputCell
deriving instance Show OutputCell
-- instance ToJSON OutputCell
instance FromJSON OutputCell

instance NFData UpdateDocument
deriving instance Generic UpdateDocument
deriving instance Show UpdateDocument
-- instance ToJSON UpdateDocument
instance FromJSON UpdateDocument

instance NFData Update
deriving instance Generic Update
deriving instance Show Update
-- instance ToJSON Update
instance FromJSON Update

instance NFData NewField
deriving instance Generic NewField
deriving instance Show NewField
-- instance ToJSON NewField
instance FromJSON NewField

instance NFData RenameField
deriving instance Generic RenameField
deriving instance Show RenameField
-- instance ToJSON RenameField
instance FromJSON RenameField

instance NFData DeleteField
deriving instance Generic DeleteField
deriving instance Show DeleteField
-- instance ToJSON DeleteField
instance FromJSON DeleteField

instance NFData UpdateCell
deriving instance Generic UpdateCell
deriving instance Show UpdateCell
-- instance ToJSON UpdateCell
instance FromJSON UpdateCell

instance NFData DeleteCell
deriving instance Generic DeleteCell
deriving instance Show DeleteCell
-- instance ToJSON DeleteCell
instance FromJSON DeleteCell

instance NFData NewCell
deriving instance Generic NewCell
deriving instance Show NewCell
-- instance ToJSON NewCell
instance FromJSON NewCell

instance NFData RenameCell
deriving instance Generic RenameCell
deriving instance Show RenameCell
-- instance ToJSON RenameCell
instance FromJSON RenameCell

instance NFData UpdatePath
deriving instance Generic UpdatePath
deriving instance Show UpdatePath
-- instance ToJSON UpdatePath
instance FromJSON UpdatePath

instance NFData PathUpdate
deriving instance Generic PathUpdate
deriving instance Show PathUpdate
-- instance ToJSON PathUpdate
instance FromJSON PathUpdate

instance NFData Removal
deriving instance Generic Removal
deriving instance Show Removal
-- instance ToJSON Removal
instance FromJSON Removal

instance NFData Code
deriving instance Generic Code
deriving instance Show Code
-- instance ToJSON Code
instance FromJSON Code

instance NFData UpdateResult
deriving instance Generic UpdateResult
deriving instance Show UpdateResult
-- instance ToJSON UpdateResult
instance FromJSON UpdateResult

instance NFData NestedCellError
deriving instance Generic NestedCellError
deriving instance Show NestedCellError
-- instance ToJSON NestedCellError
instance FromJSON NestedCellError

instance NFData MaybeRow
deriving instance Generic MaybeRow
deriving instance Show MaybeRow
-- instance ToJSON MaybeRow
instance FromJSON MaybeRow

instance NFData VariantArgument
deriving instance Generic VariantArgument
deriving instance Show VariantArgument
-- instance ToJSON VariantArgument
instance FromJSON VariantArgument

instance NFData DataPath
deriving instance Generic DataPath
deriving instance Show DataPath
-- instance ToJSON DataPath
instance FromJSON DataPath

deriving instance Real DocumentId
deriving instance Enum DocumentId
deriving instance Ord DocumentId
deriving instance Eq DocumentId
deriving instance Num DocumentId
deriving instance Integral DocumentId
instance NFData DocumentId
deriving instance Generic DocumentId
deriving instance Show DocumentId
-- instance ToJSON DocumentId
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
deriving instance Generic Version1
deriving instance Eq Version1
instance NFData Version1
instance Version Version1 where versionNumber _ = 1; versionRefl = Version1
instance FromJSON Version1 where parseJSON = parseVersion
-- instance ToJSON Version1 where toJSON = versionToJSON

deriving instance Show Version2
deriving instance Generic Version2
deriving instance Eq Version2
instance NFData Version2
instance Version Version2 where versionNumber _ = 2; versionRefl = Version2
instance FromJSON Version2 where parseJSON = parseVersion
-- instance ToJSON Version2 where toJSON = versionToJSON

$(derivePersistFieldJSON "InputDocument")
$(derivePersistFieldJSON "OutputDocument")
$(derivePersistFieldJSON "InputCell")
$(derivePersistFieldJSON "OutputCell")

$(derivePersistFieldJSON "InputDocument1")
$(derivePersistFieldJSON "InputCell1")

--------------------------------------------------------------------------------
-- Frisson

$(Frisson.deriveAll
  "../inflex-client/src/Inflex/Frisson.purs"
  "../inflex-client/src/Inflex/Frisson.js"
  "module Inflex.Frisson where\n\
   \\n\
   \import Inflex.Schema\n\
   \import Data.UUID (UUID)\n\
   \import Data.Argonaut.Core (Json)\n\
   \"
  [''UUID,''Version1,''Version2,''RefreshDocument,''UpdateDocument,''UpdateSandbox,''UpdateResult,''NestedCellError,''Update,''NewCell,''DeleteCell,''RenameCell,''UpdateCell,''UpdatePath,''PathUpdate,''Code,''Removal,''NewField,''RenameField,''DeleteField,''DataPath,''OutputDocument,''InputDocument1,''OutputCell,''InputCell1,''Result,''Tree2,''VariantArgument,''MaybeRow,''Row,''Field2,''OriginalSource,''CellError,''FillError,''FileQuery,''FilesOutput,''File,''CsvCheckStatus,''CsvImportFinal,''CsvColumnProblem,''CsvGuess,''CsvImportSpec,''CsvColumn,''ColumnAction,''ImportColumn,''CsvColumnType,''Optionality,''InputDocument,''InputCell,''Tree1,''Field1,''DocumentId,''ResultTree,''Hash])
