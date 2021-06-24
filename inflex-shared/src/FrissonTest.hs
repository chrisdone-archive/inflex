{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module FrissonTest where

import Data.Text (Text)
import Data.Vector (Vector)
import Frisson

newtype UUID = UUID Text


newtype Hash = Hash Text


{-

GUIDELINE:

* Changing input types:
  - Duplicate and bump version when adding more required fields or
    renaming fields. Removed fields can simply be ignored, but a version
    bump is explicit.

* Changing output types:
  - Duplicate and bump version when removing fields or renaming
   fields. New fields can simply be ignored, but a version bump is
   explicit.
  - A breaking change may merit an rpc endpoint clone-and-bump.

* Deprecating old rpc endpoints can use 410 gone.

* Add a warp middleware to insert a header in responses of the server
  schema version? And server commit.

* Always check your ./rpc file.

-}

--------------------------------------------------------------------------------
-- Versions

class Version v where
  versionNumber :: v -> Int
  versionRefl :: v

data Version1 = Version1
data Version2 = Version2

--------------------------------------------------------------------------------
-- Command types

data RefreshDocument = RefreshDocument
  { document :: InputDocument1
  , documentId :: DocumentId
  }

data UpdateDocument = UpdateDocument
  { documentId :: DocumentId
  , update :: Update
  }

data UpdateSandbox = UpdateSandbox
  { update :: Update
  , document :: InputDocument1
  }

data UpdateResult
  = UpdatedDocument OutputDocument
  | NestedError NestedCellError

data NestedCellError = NestedCellError
  { path :: DataPath
  , error :: CellError
  }

--------------------------------------------------------------------------------
-- Update commands
--
-- * Adding a constructor to these types does NOT need a bump. A
--   client will never send something _new_; it's always the server that
--   brings something new.
--
-- * Renaming/changing/removing a constructor WOULD require a
--   bump. (Think: client sends a constructor that the server considers
--   removed; should have a migration.)
--
-- TODO: Find a way to automate this with TH?

data Update
  = CellUpdate UpdateCell
  | CellRename RenameCell
  | CellDelete DeleteCell
  | CellNew NewCell

data NewCell = NewCell {
  code :: Text
 }

data DeleteCell = DeleteCell {
  uuid :: UUID
 }

data RenameCell = RenameCell {
  uuid :: UUID,
  newname :: Text
 }

data UpdateCell = UpdateCell {
  uuid :: UUID,
  update :: UpdatePath
 }

data UpdatePath = UpdatePath
 { path :: DataPath
 , update :: PathUpdate
 }

data PathUpdate
  = NewFieldUpdate NewField
  | RenameFieldUpdate RenameField
  | DeleteFieldUpdate DeleteField
  | RemoveUpdate Removal
    -- ^ Remove an element from the container given by index.
  | AddToEndUpdate
    -- ^ I.e. add a blank element at the end the container.
  | CodeUpdate Code

data Code = Code { text :: Text }

data Removal = Removal { index :: Int }

data NewField = NewField { name :: Text }

data RenameField = RenameField { from :: Text, to :: Text }

data DeleteField = DeleteField { name :: Text }

data DataPath
  = DataHere
  | DataElemOf Int DataPath
  | DataFieldOf Text DataPath
  | DataVariantOf Text DataPath

--------------------------------------------------------------------------------
-- General document types (not specific to a given command)

newtype DocumentId =
  DocumentId Int

data OutputDocument = OutputDocument
  { cells :: Vector OutputCell
  }

data InputDocument1 = InputDocument1
  { cells :: Vector InputCell1
  }

data OutputCell = OutputCell
  { uuid :: UUID
  , name :: Text
  , code :: Text
  , result :: Result
  , order :: Int
  , hash :: Hash
  }

data InputCell1 = InputCell1
  { uuid :: UUID
  , name :: Text
  , code :: Text
  , order :: Int
  , version :: Version1
  }

data Result
  = ResultError CellError
  | ResultOk ResultTree

newtype ResultTree =
  ResultTree Tree2

data Tree2
  = ArrayTree2 Version2 OriginalSource (Vector Tree2)
  | RecordTree2 Version2 OriginalSource (Vector Field2)
  | TableTree2 Version2 OriginalSource (Vector Text) (Vector Row)
  | TextTree2 Version2 OriginalSource Text
  | VegaTree2 Version2 OriginalSource Text
  | VariantTree2 Version2 OriginalSource Text VariantArgument
  | MiscTree2 Version2 OriginalSource Text
  | TableTreeMaybe2 Version2 OriginalSource (Vector Text) (Vector MaybeRow)

data VariantArgument =
  VariantArgument Tree2 | NoVariantArgument

data MaybeRow
  = SomeRow Row
  | HoleRow

data Row = Row
 { source :: OriginalSource
 , fields :: Vector Field2
 }

data Field2 = Field2
  { version :: Version2
  , key :: Text
  , value :: Tree2
  }

data OriginalSource
  = OriginalSource Text
  | NoOriginalSource

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
-- CSV import

data FileQuery = FileQuery
  { search :: Text
  }

data FilesOutput = FilesOutput
  { files :: Vector File
  }

data File = File
  { id :: Int
  , name :: Text
  }

data CsvCheckStatus
  = CsvParsesHappily
  | CsvColumnFailures (Vector CsvColumnProblem)

data CsvImportFinal = CsvImportFinal
  { csvImportSpec :: CsvImportSpec
  , documentId :: DocumentId
  }

data CsvColumnProblem
  = FoundNonInteger Text
  | FoundNonDecimal Text
  | RequiredColumnHasEmpty

data CsvGuess
  = CsvGuessed CsvImportSpec
  | GuessCassavaFailure Text

data CsvImportSpec = CsvImportSpec
  { file :: File
  , skipRows :: Int
  , separator :: Text
  , columns :: Vector CsvColumn
  }

data CsvColumn = CsvColumn
  { name :: Text
  , action :: ColumnAction
  }

data ColumnAction
  = IgnoreColumn
  | ImportAction ImportColumn

data ImportColumn = ImportColumn
  { importType :: CsvColumnType
  , renameTo :: Text -- No renames will just be the same name.
  }

data CsvColumnType
  = IntegerType Optionality
  | DecimalType Int Optionality
  | TextType Optionality

-- | Versions added here because there is a mismatch between
-- foreign-generic and haskell. :-(
--
-- I need to add a slot, else the decode fails.
--
-- =(
data Optionality
  = Optional
  | Required

--------------------------------------------------------------------------------
-- Deprecated -- types that should no longer be used outside of the
-- Schema.hs/.purs modules

{-# DEPRECATED InputDocument "Use InputDocument1" #-}
data InputDocument = InputDocument
  { cells :: Vector InputCell
  }

{-# DEPRECATED InputCell "Use InputCell1" #-}
data InputCell = InputCell
  { uuid :: UUID
  , name :: Text
  , code :: Text
  }

{-# DEPRECATED Tree1 "Use Tree2" #-}
data Tree1
  = ArrayTree Version1 (Vector Tree1)
  | RecordTree Version1 (Vector Field1)
  | MiscTree Version1 Text

{-# DEPRECATED Field1 "Use Field2" #-}
data Field1 = Field1
  { version :: Version1
  , key :: Text
  , value :: Tree1
  }

$(Frisson.derive ''UUID)
$(Frisson.derive ''OutputDocument)
$(Frisson.derive ''Result)
$(Frisson.derive ''Optionality)
