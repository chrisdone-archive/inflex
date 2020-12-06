-- | Shared data types.

module Inflex.Schema where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.UUID (UUID)
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Generic (class Decode, class Encode, decode, encode, genericDecode, genericEncode)
import Inflex.Json (opts)
import Prelude

--------------------------------------------------------------------------------
-- Types

type Vector a = Array a

type Text = String

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
-- Basic types

data None =
  None

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
  | DataFieldOf Int DataPath

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
  | MiscTree2 Version2 OriginalSource Text

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

derive instance genericNone :: Generic None _
instance showNone :: Show None where show = genericShow
instance decodeNone :: Decode None where decode = genericDecode opts
instance encodeNone :: Encode None where encode = genericEncode opts

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

derive instance genericInputCell :: Generic InputCell _
instance showInputCell :: Show InputCell where show = genericShow
instance decodeInputCell :: Decode InputCell where decode = genericDecode opts
instance encodeInputCell :: Encode InputCell where encode = genericEncode opts

derive instance genericInputCell1 :: Generic InputCell1 _
instance showInputCell1 :: Show InputCell1 where show = genericShow
instance decodeInputCell1 :: Decode InputCell1 where decode = genericDecode opts
instance encodeInputCell1 :: Encode InputCell1 where encode = genericEncode opts

derive instance genericOutputCell :: Generic OutputCell _
instance showOutputCell :: Show OutputCell where show = genericShow
instance decodeOutputCell :: Decode OutputCell where decode = genericDecode opts
instance encodeOutputCell :: Encode OutputCell where encode = genericEncode opts

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
