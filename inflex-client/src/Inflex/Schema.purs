-- | Shared data types.

module Inflex.Schema where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Eq (genericEq)
import Data.UUID (UUID)
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Generic (class Decode, class Encode, decode, encode, genericDecode, genericEncode)
import Inflex.Json (opts)
import Prelude

--------------------------------------------------------------------------------
-- Types

type Vector a = Array a

type Text = String

newtype Hash = Hash String

--------------------------------------------------------------------------------
-- Custom types

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
  , seen :: Vector Hash
  }

data TravelDocument = TravelDocument
  { documentId :: DocumentId
  , seen :: Vector Hash
  }

data UpdateSandbox = UpdateSandbox
  { update :: Update
  , document :: InputDocument1
  -- TODO: Add 'seen' field?
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
  | CellReposition RepositionCell
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

data RepositionCell = RepositionCell {
  uuid :: UUID,
  x :: Int,
  y :: Int
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
  { cells :: Vector CachedOutputCell
  }

data InputDocument1 = InputDocument1
  { cells :: Vector InputCell1
  }

data CachedText = FreshText Text Hash | CachedText Hash

data CachedResult = FreshResult Result Hash | CachedResult Hash

data CachedOutputCell = CachedOutputCell
  { uuid :: UUID
  , name :: Text
  , code :: CachedText
  , result :: CachedResult
  , order :: Int
  , position :: Position
  , dependencies :: Vector UUID
  }

data InputCell1 = InputCell1
  { uuid :: UUID
  , name :: Text
  , code :: Text
  , order :: Int
  , version :: Version1
  , position :: Position
  }

data Position = Unpositioned | AbsolutePosition Int Int

data Result
  = ResultError CellError
  | ResultOk ResultTree

data ResultTree = ResultTree
  { tree :: Tree2
  , typ :: TypeOf
  }

data TypeOf
  = ArrayOf TypeOf
  | RecordOf (Vector NamedType)
  | TableOf (Vector NamedType) -- TODO: this isn't being produced by the server. remove it
  | VariantOf (Vector NamedType) OpenClosed
  | TextOf
  | MiscType

data OpenClosed
  = Open
  | Closed

data NamedType = NamedType
  { name :: Text
  , typ :: TypeOf
  }

data Tree2
  = ArrayTree2 Version2 OriginalSource (Vector Tree2)
  | RecordTree2 Version2 OriginalSource (Vector Field2)
  | TextTree2 Version2 OriginalSource Text
  | VegaTree2 Version2 OriginalSource Text
  | VariantTree2 Version2 OriginalSource Text VariantArgument
  | MiscTree2 Version2 OriginalSource Text
  | TableTreeMaybe2 Version2 OriginalSource (Vector Text) (Vector MaybeRow)
  | HoleTree OriginalSource

data VariantArgument =
  VariantArgument Tree2 | NoVariantArgument

data MaybeRow
  = SomeRow Row
  | HoleRow Tree2

data Row = Row
 { source :: OriginalSource
 , fields :: Vector Tree2
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
  = Optional Version1
  | Required Version1

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


--------------------------------------------------------------------------------
-- Derivings

derive instance genericOptionality :: Generic Optionality _
instance showOptionality :: Show Optionality where show = genericShow
instance decodeOptionality :: Decode Optionality where decode = genericDecode opts
instance encodeOptionality :: Encode Optionality where encode = genericEncode opts

derive instance genericHash :: Generic Hash _
instance showHash :: Show Hash where show = genericShow
instance eqHash :: Eq Hash where eq = genericEq
instance ordHash :: Ord Hash where compare = genericCompare
instance decodeHash :: Decode Hash where decode = genericDecode opts
instance encodeHash :: Encode Hash where encode = genericEncode opts

derive instance genericFileQuery :: Generic FileQuery _
instance showFileQuery :: Show FileQuery where show = genericShow
instance decodeFileQuery :: Decode FileQuery where decode = genericDecode opts
instance encodeFileQuery :: Encode FileQuery where encode = genericEncode opts

derive instance genericFile :: Generic File _
instance showFile :: Show File where show = genericShow
instance decodeFile :: Decode File where decode = genericDecode opts
instance encodeFile :: Encode File where encode = genericEncode opts

derive instance genericTypeOf :: Generic TypeOf _
instance showTypeOf :: Show TypeOf where show x = genericShow x
instance decodeTypeOf :: Decode TypeOf where decode x = genericDecode opts x
instance encodeTypeOf :: Encode TypeOf where encode x = genericEncode opts x

derive instance genericNamedType :: Generic NamedType _
instance showNamedType :: Show NamedType where show = genericShow
instance decodeNamedType :: Decode NamedType where decode = genericDecode opts
instance encodeNamedType :: Encode NamedType where encode = genericEncode opts

derive instance genericOpenClosed :: Generic OpenClosed _
instance showOpenClosed :: Show OpenClosed where show = genericShow
instance decodeOpenClosed :: Decode OpenClosed where decode = genericDecode opts
instance encodeOpenClosed :: Encode OpenClosed where encode = genericEncode opts

derive instance genericCsvImportFinal :: Generic CsvImportFinal _
instance showCsvImportFinal :: Show CsvImportFinal where show = genericShow
instance decodeCsvImportFinal :: Decode CsvImportFinal where decode = genericDecode opts
instance encodeCsvImportFinal :: Encode CsvImportFinal where encode = genericEncode opts

derive instance genericCsvGuess :: Generic CsvGuess _
instance showCsvGuess :: Show CsvGuess where show = genericShow
instance decodeCsvGuess :: Decode CsvGuess where decode = genericDecode opts
instance encodeCsvGuess :: Encode CsvGuess where encode = genericEncode opts

derive instance genericFilesOutput :: Generic FilesOutput _
instance showFilesOutput :: Show FilesOutput where show = genericShow
instance decodeFilesOutput :: Decode FilesOutput where decode = genericDecode opts
instance encodeFilesOutput :: Encode FilesOutput where encode = genericEncode opts

derive instance genericCsvImportSpec :: Generic CsvImportSpec _
instance showCsvImportSpec :: Show CsvImportSpec where show = genericShow
instance decodeCsvImportSpec :: Decode CsvImportSpec where decode = genericDecode opts
instance encodeCsvImportSpec :: Encode CsvImportSpec where encode = genericEncode opts

derive instance genericCsvCheckStatus :: Generic CsvCheckStatus _
instance showCsvCheckStatus :: Show CsvCheckStatus where show = genericShow
instance decodeCsvCheckStatus :: Decode CsvCheckStatus where decode = genericDecode opts
instance encodeCsvCheckStatus :: Encode CsvCheckStatus where encode = genericEncode opts

derive instance genericCsvColumnProblem :: Generic CsvColumnProblem _
instance showCsvColumnProblem :: Show CsvColumnProblem where show = genericShow
instance decodeCsvColumnProblem :: Decode CsvColumnProblem where decode = genericDecode opts
instance encodeCsvColumnProblem :: Encode CsvColumnProblem where encode = genericEncode opts

derive instance genericCsvColumn :: Generic CsvColumn _
instance showCsvColumn :: Show CsvColumn where show = genericShow
instance decodeCsvColumn :: Decode CsvColumn where decode = genericDecode opts
instance encodeCsvColumn :: Encode CsvColumn where encode = genericEncode opts

derive instance genericCsvColumnType :: Generic CsvColumnType _
instance showCsvColumnType :: Show CsvColumnType where show = genericShow
instance decodeCsvColumnType :: Decode CsvColumnType where decode = genericDecode opts
instance encodeCsvColumnType :: Encode CsvColumnType where encode = genericEncode opts

derive instance genericColumnAction :: Generic ColumnAction _
instance showColumnAction :: Show ColumnAction where show = genericShow
instance decodeColumnAction :: Decode ColumnAction where decode = genericDecode opts
instance encodeColumnAction :: Encode ColumnAction where encode = genericEncode opts

derive instance genericImportColumn :: Generic ImportColumn _
instance showImportColumn :: Show ImportColumn where show = genericShow
instance decodeImportColumn :: Decode ImportColumn where decode = genericDecode opts
instance encodeImportColumn :: Encode ImportColumn where encode = genericEncode opts

derive instance genericDataPath :: Generic DataPath _
derive instance eqDataPath :: Eq DataPath
instance showDataPath :: Show DataPath where show x = genericShow x
instance decodeDataPath :: Decode DataPath where decode x = genericDecode opts x
instance encodeDataPath :: Encode DataPath where encode x = genericEncode opts x

derive instance genericResult :: Generic Result _
instance showResult :: Show Result where show = genericShow
instance decodeResult :: Decode Result where decode = genericDecode opts
instance encodeResult :: Encode Result where encode = genericEncode opts

derive instance genericTree1 :: Generic Tree1 _
instance showTree1 :: Show Tree1 where show x = genericShow x
instance decodeTree1 :: Decode Tree1 where decode x = genericDecode opts x
instance encodeTree1 :: Encode Tree1 where encode x = genericEncode opts x

derive instance genericTree2 :: Generic Tree2 _
instance showTree2 :: Show Tree2 where show x = genericShow x
instance decodeTree2 :: Decode Tree2 where decode x = genericDecode opts x
instance encodeTree2 :: Encode Tree2 where encode x = genericEncode opts x

derive instance genericResultTree :: Generic ResultTree _
instance showResultTree :: Show ResultTree where show x = genericShow x
instance encodeResultTree :: Encode ResultTree where encode (ResultTree tree) = encode tree
instance decodeResultTree :: Decode ResultTree where decode x = genericDecode opts x

derive instance genericCellError :: Generic CellError _
instance showCellError :: Show CellError where show = genericShow
instance decodeCellError :: Decode CellError where decode = genericDecode opts
instance encodeCellError :: Encode CellError where encode = genericEncode opts

derive instance genericField1 :: Generic Field1 _
instance showField1 :: Show Field1 where show x = genericShow x
instance decodeField1 :: Decode Field1 where decode x = genericDecode opts x
instance encodeField1 :: Encode Field1 where encode x = genericEncode opts x

derive instance genericField2 :: Generic Field2 _
instance showField2 :: Show Field2 where show x = genericShow x
instance decodeField2 :: Decode Field2 where decode x = genericDecode opts x
instance encodeField2 :: Encode Field2 where encode x = genericEncode opts x

derive instance genericRow :: Generic Row _
instance showRow :: Show Row where show x = genericShow x
instance decodeRow :: Decode Row where decode x = genericDecode opts x
instance encodeRow :: Encode Row where encode x = genericEncode opts x

derive instance genericFillError :: Generic FillError _
instance showFillError :: Show FillError where show = genericShow
instance decodeFillError :: Decode FillError where decode = genericDecode opts
instance encodeFillError :: Encode FillError where encode = genericEncode opts

derive instance genericOriginalSource :: Generic OriginalSource _
instance showOriginalSource :: Show OriginalSource where show = genericShow
instance decodeOriginalSource :: Decode OriginalSource where decode = genericDecode opts
instance encodeOriginalSource :: Encode OriginalSource where encode = genericEncode opts

derive instance genericInputDocument :: Generic InputDocument _
instance showInputDocument :: Show InputDocument where show = genericShow
instance decodeInputDocument :: Decode InputDocument where decode = genericDecode opts
instance encodeInputDocument :: Encode InputDocument where encode = genericEncode opts

derive instance genericInputDocument1 :: Generic InputDocument1 _
instance showInputDocument1 :: Show InputDocument1 where show = genericShow
instance decodeInputDocument1 :: Decode InputDocument1 where decode = genericDecode opts
instance encodeInputDocument1 :: Encode InputDocument1 where encode = genericEncode opts

derive instance genericRefreshDocument :: Generic RefreshDocument _
instance showRefreshDocument :: Show RefreshDocument where show = genericShow
instance decodeRefreshDocument :: Decode RefreshDocument where decode = genericDecode opts
instance encodeRefreshDocument :: Encode RefreshDocument where encode = genericEncode opts

derive instance genericOutputDocument :: Generic OutputDocument _
instance showOutputDocument :: Show OutputDocument where show = genericShow
instance decodeOutputDocument :: Decode OutputDocument where decode = genericDecode opts
instance encodeOutputDocument :: Encode OutputDocument where encode = genericEncode opts

derive instance genericUpdateDocument :: Generic UpdateDocument _
instance showUpdateDocument :: Show UpdateDocument where show = genericShow
instance decodeUpdateDocument :: Decode UpdateDocument where decode = genericDecode opts
instance encodeUpdateDocument :: Encode UpdateDocument where encode = genericEncode opts

derive instance genericTravelDocument :: Generic TravelDocument _
instance showTravelDocument :: Show TravelDocument where show = genericShow
instance decodeTravelDocument :: Decode TravelDocument where decode = genericDecode opts
instance encodeTravelDocument :: Encode TravelDocument where encode = genericEncode opts

derive instance genericUpdateSandbox :: Generic UpdateSandbox _
instance showUpdateSandbox :: Show UpdateSandbox where show = genericShow
instance decodeUpdateSandbox :: Decode UpdateSandbox where decode = genericDecode opts
instance encodeUpdateSandbox :: Encode UpdateSandbox where encode = genericEncode opts

derive instance genericNewField :: Generic NewField _
instance showNewField :: Show NewField where show = genericShow
instance decodeNewField :: Decode NewField where decode = genericDecode opts
instance encodeNewField :: Encode NewField where encode = genericEncode opts

derive instance genericRenameField :: Generic RenameField _
instance showRenameField :: Show RenameField where show = genericShow
instance decodeRenameField :: Decode RenameField where decode = genericDecode opts
instance encodeRenameField :: Encode RenameField where encode = genericEncode opts

derive instance genericDeleteField :: Generic DeleteField _
instance showDeleteField :: Show DeleteField where show = genericShow
instance decodeDeleteField :: Decode DeleteField where decode = genericDecode opts
instance encodeDeleteField :: Encode DeleteField where encode = genericEncode opts

derive instance genericUpdate :: Generic Update _
instance showUpdate :: Show Update where show = genericShow
instance decodeUpdate :: Decode Update where decode = genericDecode opts
instance encodeUpdate :: Encode Update where encode = genericEncode opts

derive instance genericUpdateCell :: Generic UpdateCell _
instance showUpdateCell :: Show UpdateCell where show = genericShow
instance decodeUpdateCell :: Decode UpdateCell where decode = genericDecode opts
instance encodeUpdateCell :: Encode UpdateCell where encode = genericEncode opts

derive instance genericDeleteCell :: Generic DeleteCell _
instance showDeleteCell :: Show DeleteCell where show = genericShow
instance decodeDeleteCell :: Decode DeleteCell where decode = genericDecode opts
instance encodeDeleteCell :: Encode DeleteCell where encode = genericEncode opts

derive instance genericNewCell :: Generic NewCell _
instance showNewCell :: Show NewCell where show = genericShow
instance decodeNewCell :: Decode NewCell where decode = genericDecode opts
instance encodeNewCell :: Encode NewCell where encode = genericEncode opts

derive instance genericRenameCell :: Generic RenameCell _
instance showRenameCell :: Show RenameCell where show = genericShow
instance decodeRenameCell :: Decode RenameCell where decode = genericDecode opts
instance encodeRenameCell :: Encode RenameCell where encode = genericEncode opts

derive instance genericRepositionCell :: Generic RepositionCell _
instance showRepositionCell :: Show RepositionCell where show = genericShow
instance decodeRepositionCell :: Decode RepositionCell where decode = genericDecode opts
instance encodeRepositionCell :: Encode RepositionCell where encode = genericEncode opts

derive instance genericPosition :: Generic Position _
instance showPosition :: Show Position where show = genericShow
instance decodePosition :: Decode Position where decode = genericDecode opts
instance encodePosition :: Encode Position where encode = genericEncode opts

derive instance genericUpdatePath :: Generic UpdatePath _
instance showUpdatePath :: Show UpdatePath where show = genericShow
instance decodeUpdatePath :: Decode UpdatePath where decode = genericDecode opts
instance encodeUpdatePath :: Encode UpdatePath where encode = genericEncode opts

derive instance genericPathUpdate :: Generic PathUpdate _
instance showPathUpdate :: Show PathUpdate where show = genericShow
instance decodePathUpdate :: Decode PathUpdate where decode = genericDecode opts
instance encodePathUpdate :: Encode PathUpdate where encode = genericEncode opts

derive instance genericRemoval :: Generic Removal _
instance showRemoval :: Show Removal where show = genericShow
instance decodeRemoval :: Decode Removal where decode = genericDecode opts
instance encodeRemoval :: Encode Removal where encode = genericEncode opts

derive instance genericCode :: Generic Code _
instance showCode :: Show Code where show = genericShow
instance decodeCode :: Decode Code where decode = genericDecode opts
instance encodeCode :: Encode Code where encode = genericEncode opts

derive instance genericUpdateResult :: Generic UpdateResult _
instance showUpdateResult :: Show UpdateResult where show = genericShow
instance decodeUpdateResult :: Decode UpdateResult where decode = genericDecode opts
instance encodeUpdateResult :: Encode UpdateResult where encode = genericEncode opts

derive instance genericNestedCellError :: Generic NestedCellError _
instance showNestedCellError :: Show NestedCellError where show = genericShow
instance decodeNestedCellError :: Decode NestedCellError where decode = genericDecode opts
instance encodeNestedCellError :: Encode NestedCellError where encode = genericEncode opts

derive instance genericVariantArgument :: Generic VariantArgument _
instance showVariantArgument :: Show VariantArgument where show = genericShow
instance decodeVariantArgument :: Decode VariantArgument where decode = genericDecode opts
instance encodeVariantArgument :: Encode VariantArgument where encode = genericEncode opts

derive instance genericMaybeRow :: Generic MaybeRow _
instance showMaybeRow :: Show MaybeRow where show = genericShow
instance decodeMaybeRow :: Decode MaybeRow where decode = genericDecode opts
instance encodeMaybeRow :: Encode MaybeRow where encode = genericEncode opts

derive instance genericInputCell :: Generic InputCell _
instance showInputCell :: Show InputCell where show = genericShow
instance decodeInputCell :: Decode InputCell where decode = genericDecode opts
instance encodeInputCell :: Encode InputCell where encode = genericEncode opts

derive instance genericInputCell1 :: Generic InputCell1 _
instance showInputCell1 :: Show InputCell1 where show = genericShow
instance decodeInputCell1 :: Decode InputCell1 where decode = genericDecode opts
instance encodeInputCell1 :: Encode InputCell1 where encode = genericEncode opts

derive instance genericCachedOutputCell :: Generic CachedOutputCell _
instance showCachedOutputCell :: Show CachedOutputCell where show = genericShow
instance decodeCachedOutputCell :: Decode CachedOutputCell where decode = genericDecode opts
instance encodeCachedOutputCell :: Encode CachedOutputCell where encode = genericEncode opts

derive instance genericCachedText :: Generic CachedText _
instance showCachedText :: Show CachedText where show = genericShow
instance decodeCachedText :: Decode CachedText where decode = genericDecode opts
instance encodeCachedText :: Encode CachedText where encode = genericEncode opts

derive instance genericCachedResult :: Generic CachedResult _
instance showCachedResult :: Show CachedResult where show = genericShow
instance decodeCachedResult :: Decode CachedResult where decode = genericDecode opts
instance encodeCachedResult :: Encode CachedResult where encode = genericEncode opts

derive instance genericDocumentId :: Generic DocumentId _
instance showDocumentId :: Show DocumentId where show = genericShow
instance decodeDocumentId :: Decode DocumentId where decode = genericDecode opts
instance encodeDocumentId :: Encode DocumentId where encode = genericEncode opts

--------------------------------------------------------------------------------
-- Version infra

parseVersion :: forall v. Version v => Foreign -> F v
parseVersion j = do
  i <- decode j
  if i == versionNumber (versionRefl :: v)
    then pure (versionRefl :: v)
    else fail
           (TypeMismatch
              ("Version" <> show (versionNumber (versionRefl :: v)))
              ("Version" <> show i))

versionToJSON :: forall v. Version v => v -> Foreign
versionToJSON v = encode (versionNumber v)

--------------------------------------------------------------------------------
-- Versions

instance versionVersion1 :: Version Version1 where
  versionNumber _ = 1
  versionRefl = Version1
derive instance genericVersion1 :: Generic Version1 _
instance showVersion1 :: Show Version1 where show = genericShow
instance decodeVersion1 :: Decode Version1 where decode = parseVersion
instance encodeVersion1 :: Encode Version1 where encode = versionToJSON

instance versionVersion2 :: Version Version2 where
  versionNumber _ = 2
  versionRefl = Version2
derive instance genericVersion2 :: Generic Version2 _
instance showVersion2 :: Show Version2 where show = genericShow
instance decodeVersion2 :: Decode Version2 where decode = parseVersion
instance encodeVersion2 :: Encode Version2 where encode = versionToJSON
