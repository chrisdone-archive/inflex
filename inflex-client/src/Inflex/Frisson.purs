module Inflex.Frisson where

import Inflex.Schema
import Data.UUID (UUID)
import Data.Argonaut.Core (Json)
import Prelude (class Show)
foreign import data View :: Type -> Type

foreign import showView :: forall a. View a -> String

instance showViewOf :: Show (View a) where show = showView

foreign import unsafeView :: forall a. Json -> View a

foreign import viewUUID :: Json -> (View UUID)

foreign import unviewUUID :: (View UUID) -> Json

foreign import unUUID :: (View UUID) -> String

foreign import viewRefreshDocument :: Json -> (View RefreshDocument)

foreign import unviewRefreshDocument :: (View RefreshDocument) -> Json

foreign import refreshDocumentDocument :: (View RefreshDocument) -> (View InputDocument1)

foreign import refreshDocumentDocumentId :: (View RefreshDocument) -> (View DocumentId)

foreign import viewUpdateDocument :: Json -> (View UpdateDocument)

foreign import unviewUpdateDocument :: (View UpdateDocument) -> Json

foreign import updateDocumentDocumentId :: (View UpdateDocument) -> (View DocumentId)

foreign import updateDocumentUpdate :: (View UpdateDocument) -> (View Update)

foreign import updateDocumentSeen :: (View UpdateDocument) -> (Array (View Hash))

foreign import viewUpdateSandbox :: Json -> (View UpdateSandbox)

foreign import unviewUpdateSandbox :: (View UpdateSandbox) -> Json

foreign import updateSandboxUpdate :: (View UpdateSandbox) -> (View Update)

foreign import updateSandboxDocument :: (View UpdateSandbox) -> (View InputDocument1)

foreign import viewUpdateResult :: Json -> (View UpdateResult)

foreign import unviewUpdateResult :: (View UpdateResult) -> Json

foreign import caseUpdateResult :: forall r. {
  "UpdatedDocument" :: (View OutputDocument) -> r,
  "NestedError" :: (View NestedCellError) -> r
  } -> (View UpdateResult) -> r

foreign import viewNestedCellError :: Json -> (View NestedCellError)

foreign import unviewNestedCellError :: (View NestedCellError) -> Json

foreign import nestedCellErrorPath :: (View NestedCellError) -> (View DataPath)

foreign import nestedCellErrorError :: (View NestedCellError) -> (View CellError)

foreign import viewUpdate :: Json -> (View Update)

foreign import unviewUpdate :: (View Update) -> Json

foreign import caseUpdate :: forall r. {
  "CellUpdate" :: (View UpdateCell) -> r,
  "CellRename" :: (View RenameCell) -> r,
  "CellDelete" :: (View DeleteCell) -> r,
  "CellNew" :: (View NewCell) -> r
  } -> (View Update) -> r

foreign import viewNewCell :: Json -> (View NewCell)

foreign import unviewNewCell :: (View NewCell) -> Json

foreign import newCellCode :: (View NewCell) -> String

foreign import viewDeleteCell :: Json -> (View DeleteCell)

foreign import unviewDeleteCell :: (View DeleteCell) -> Json

foreign import deleteCellUuid :: (View DeleteCell) -> (View UUID)

foreign import viewRenameCell :: Json -> (View RenameCell)

foreign import unviewRenameCell :: (View RenameCell) -> Json

foreign import renameCellUuid :: (View RenameCell) -> (View UUID)

foreign import renameCellNewname :: (View RenameCell) -> String

foreign import viewUpdateCell :: Json -> (View UpdateCell)

foreign import unviewUpdateCell :: (View UpdateCell) -> Json

foreign import updateCellUuid :: (View UpdateCell) -> (View UUID)

foreign import updateCellUpdate :: (View UpdateCell) -> (View UpdatePath)

foreign import viewUpdatePath :: Json -> (View UpdatePath)

foreign import unviewUpdatePath :: (View UpdatePath) -> Json

foreign import updatePathPath :: (View UpdatePath) -> (View DataPath)

foreign import updatePathUpdate :: (View UpdatePath) -> (View PathUpdate)

foreign import viewPathUpdate :: Json -> (View PathUpdate)

foreign import unviewPathUpdate :: (View PathUpdate) -> Json

foreign import casePathUpdate :: forall r. {
  "NewFieldUpdate" :: (View NewField) -> r,
  "RenameFieldUpdate" :: (View RenameField) -> r,
  "DeleteFieldUpdate" :: (View DeleteField) -> r,
  "RemoveUpdate" :: (View Removal) -> r,
  "AddToEndUpdate" :: r,
  "CodeUpdate" :: (View Code) -> r
  } -> (View PathUpdate) -> r

foreign import viewCode :: Json -> (View Code)

foreign import unviewCode :: (View Code) -> Json

foreign import codeText :: (View Code) -> String

foreign import viewRemoval :: Json -> (View Removal)

foreign import unviewRemoval :: (View Removal) -> Json

foreign import removalIndex :: (View Removal) -> Int

foreign import viewNewField :: Json -> (View NewField)

foreign import unviewNewField :: (View NewField) -> Json

foreign import newFieldName :: (View NewField) -> String

foreign import viewRenameField :: Json -> (View RenameField)

foreign import unviewRenameField :: (View RenameField) -> Json

foreign import renameFieldFrom :: (View RenameField) -> String

foreign import renameFieldTo :: (View RenameField) -> String

foreign import viewDeleteField :: Json -> (View DeleteField)

foreign import unviewDeleteField :: (View DeleteField) -> Json

foreign import deleteFieldName :: (View DeleteField) -> String

foreign import viewDataPath :: Json -> (View DataPath)

foreign import unviewDataPath :: (View DataPath) -> Json

foreign import caseDataPath :: forall r. {
  "DataHere" :: r,
  "DataElemOf" :: Int -> (View DataPath) -> r,
  "DataFieldOf" :: String -> (View DataPath) -> r,
  "DataVariantOf" :: String -> (View DataPath) -> r
  } -> (View DataPath) -> r

foreign import viewOutputDocument :: Json -> (View OutputDocument)

foreign import unviewOutputDocument :: (View OutputDocument) -> Json

foreign import outputDocumentCells :: (View OutputDocument) -> (Array (View CachedOutputCell))

foreign import viewInputDocument1 :: Json -> (View InputDocument1)

foreign import unviewInputDocument1 :: (View InputDocument1) -> Json

foreign import inputDocument1Cells :: (View InputDocument1) -> (Array (View InputCell1))

foreign import viewCachedOutputCell :: Json -> (View CachedOutputCell)

foreign import unviewCachedOutputCell :: (View CachedOutputCell) -> Json

foreign import cachedOutputCellUuid :: (View CachedOutputCell) -> (View UUID)

foreign import cachedOutputCellName :: (View CachedOutputCell) -> String

foreign import cachedOutputCellCode :: (View CachedOutputCell) -> (View CachedText)

foreign import cachedOutputCellResult :: (View CachedOutputCell) -> (View CachedResult)

foreign import cachedOutputCellOrder :: (View CachedOutputCell) -> Int

foreign import viewInputCell1 :: Json -> (View InputCell1)

foreign import unviewInputCell1 :: (View InputCell1) -> Json

foreign import inputCell1Uuid :: (View InputCell1) -> (View UUID)

foreign import inputCell1Name :: (View InputCell1) -> String

foreign import inputCell1Code :: (View InputCell1) -> String

foreign import inputCell1Order :: (View InputCell1) -> Int

foreign import inputCell1Version :: (View InputCell1) -> (View Version1)

foreign import viewResult :: Json -> (View Result)

foreign import unviewResult :: (View Result) -> Json

foreign import caseResult :: forall r. {
  "ResultError" :: (View CellError) -> r,
  "ResultOk" :: (View ResultTree) -> r
  } -> (View Result) -> r

foreign import viewTree2 :: Json -> (View Tree2)

foreign import unviewTree2 :: (View Tree2) -> Json

foreign import caseTree2 :: forall r. {
  "ArrayTree2" :: (View Version2) -> (View OriginalSource) -> (Array (View Tree2)) -> r,
  "RecordTree2" :: (View Version2) -> (View OriginalSource) -> (Array (View Field2)) -> r,
  "TextTree2" :: (View Version2) -> (View OriginalSource) -> String -> r,
  "VegaTree2" :: (View Version2) -> (View OriginalSource) -> String -> r,
  "VariantTree2" :: (View Version2) -> (View OriginalSource) -> String -> (View VariantArgument) -> r,
  "MiscTree2" :: (View Version2) -> (View OriginalSource) -> String -> r,
  "TableTreeMaybe2" :: (View Version2) -> (View OriginalSource) -> (Array String) -> (Array (View MaybeRow)) -> r,
  "HoleTree" :: (View OriginalSource) -> r
  } -> (View Tree2) -> r

foreign import viewVariantArgument :: Json -> (View VariantArgument)

foreign import unviewVariantArgument :: (View VariantArgument) -> Json

foreign import caseVariantArgument :: forall r. {
  "VariantArgument" :: (View Tree2) -> r,
  "NoVariantArgument" :: r
  } -> (View VariantArgument) -> r

foreign import viewMaybeRow :: Json -> (View MaybeRow)

foreign import unviewMaybeRow :: (View MaybeRow) -> Json

foreign import caseMaybeRow :: forall r. {
  "SomeRow" :: (View Row) -> r,
  "HoleRow" :: (View Tree2) -> r
  } -> (View MaybeRow) -> r

foreign import viewRow :: Json -> (View Row)

foreign import unviewRow :: (View Row) -> Json

foreign import rowSource :: (View Row) -> (View OriginalSource)

foreign import rowFields :: (View Row) -> (Array (View Tree2))

foreign import viewField2 :: Json -> (View Field2)

foreign import unviewField2 :: (View Field2) -> Json

foreign import field2Version :: (View Field2) -> (View Version2)

foreign import field2Key :: (View Field2) -> String

foreign import field2Value :: (View Field2) -> (View Tree2)

foreign import viewOriginalSource :: Json -> (View OriginalSource)

foreign import unviewOriginalSource :: (View OriginalSource) -> Json

foreign import caseOriginalSource :: forall r. {
  "OriginalSource" :: String -> r,
  "NoOriginalSource" :: r
  } -> (View OriginalSource) -> r

foreign import viewCellError :: Json -> (View CellError)

foreign import unviewCellError :: (View CellError) -> Json

foreign import caseCellError :: forall r. {
  "SyntaxError" :: r,
  "FillErrors" :: (Array (View FillError)) -> r,
  "CyclicCells" :: (Array String) -> r,
  "DuplicateCellName" :: r,
  "CellRenameErrors" :: r,
  "CellTypeError" :: r,
  "CellStepEror" :: r
  } -> (View CellError) -> r

foreign import viewFillError :: Json -> (View FillError)

foreign import unviewFillError :: (View FillError) -> Json

foreign import caseFillError :: forall r. {
  "NoSuchGlobal" :: String -> r,
  "OtherCellProblem" :: String -> r
  } -> (View FillError) -> r

foreign import viewFileQuery :: Json -> (View FileQuery)

foreign import unviewFileQuery :: (View FileQuery) -> Json

foreign import fileQuerySearch :: (View FileQuery) -> String

foreign import viewFilesOutput :: Json -> (View FilesOutput)

foreign import unviewFilesOutput :: (View FilesOutput) -> Json

foreign import filesOutputFiles :: (View FilesOutput) -> (Array (View File))

foreign import viewFile :: Json -> (View File)

foreign import unviewFile :: (View File) -> Json

foreign import fileId :: (View File) -> Int

foreign import fileName :: (View File) -> String

foreign import viewCsvCheckStatus :: Json -> (View CsvCheckStatus)

foreign import unviewCsvCheckStatus :: (View CsvCheckStatus) -> Json

foreign import caseCsvCheckStatus :: forall r. {
  "CsvParsesHappily" :: r,
  "CsvColumnFailures" :: (Array (View CsvColumnProblem)) -> r
  } -> (View CsvCheckStatus) -> r

foreign import viewCsvImportFinal :: Json -> (View CsvImportFinal)

foreign import unviewCsvImportFinal :: (View CsvImportFinal) -> Json

foreign import csvImportFinalCsvImportSpec :: (View CsvImportFinal) -> (View CsvImportSpec)

foreign import csvImportFinalDocumentId :: (View CsvImportFinal) -> (View DocumentId)

foreign import viewCsvColumnProblem :: Json -> (View CsvColumnProblem)

foreign import unviewCsvColumnProblem :: (View CsvColumnProblem) -> Json

foreign import caseCsvColumnProblem :: forall r. {
  "FoundNonInteger" :: String -> r,
  "FoundNonDecimal" :: String -> r,
  "RequiredColumnHasEmpty" :: r
  } -> (View CsvColumnProblem) -> r

foreign import viewCsvGuess :: Json -> (View CsvGuess)

foreign import unviewCsvGuess :: (View CsvGuess) -> Json

foreign import caseCsvGuess :: forall r. {
  "CsvGuessed" :: (View CsvImportSpec) -> r,
  "GuessCassavaFailure" :: String -> r
  } -> (View CsvGuess) -> r

foreign import viewCsvImportSpec :: Json -> (View CsvImportSpec)

foreign import unviewCsvImportSpec :: (View CsvImportSpec) -> Json

foreign import csvImportSpecFile :: (View CsvImportSpec) -> (View File)

foreign import csvImportSpecSkipRows :: (View CsvImportSpec) -> Int

foreign import csvImportSpecSeparator :: (View CsvImportSpec) -> String

foreign import csvImportSpecColumns :: (View CsvImportSpec) -> (Array (View CsvColumn))

foreign import viewCsvColumn :: Json -> (View CsvColumn)

foreign import unviewCsvColumn :: (View CsvColumn) -> Json

foreign import csvColumnName :: (View CsvColumn) -> String

foreign import csvColumnAction :: (View CsvColumn) -> (View ColumnAction)

foreign import viewColumnAction :: Json -> (View ColumnAction)

foreign import unviewColumnAction :: (View ColumnAction) -> Json

foreign import caseColumnAction :: forall r. {
  "IgnoreColumn" :: r,
  "ImportAction" :: (View ImportColumn) -> r
  } -> (View ColumnAction) -> r

foreign import viewImportColumn :: Json -> (View ImportColumn)

foreign import unviewImportColumn :: (View ImportColumn) -> Json

foreign import importColumnImportType :: (View ImportColumn) -> (View CsvColumnType)

foreign import importColumnRenameTo :: (View ImportColumn) -> String

foreign import viewCsvColumnType :: Json -> (View CsvColumnType)

foreign import unviewCsvColumnType :: (View CsvColumnType) -> Json

foreign import caseCsvColumnType :: forall r. {
  "IntegerType" :: (View Optionality) -> r,
  "DecimalType" :: Int -> (View Optionality) -> r,
  "TextType" :: (View Optionality) -> r
  } -> (View CsvColumnType) -> r

foreign import viewOptionality :: Json -> (View Optionality)

foreign import unviewOptionality :: (View Optionality) -> Json

foreign import caseOptionality :: forall r. {
  "Optional" :: (View Version1) -> r,
  "Required" :: (View Version1) -> r
  } -> (View Optionality) -> r

foreign import viewInputDocument :: Json -> (View InputDocument)

foreign import unviewInputDocument :: (View InputDocument) -> Json

foreign import inputDocumentCells :: (View InputDocument) -> (Array (View InputCell))

foreign import viewInputCell :: Json -> (View InputCell)

foreign import unviewInputCell :: (View InputCell) -> Json

foreign import inputCellUuid :: (View InputCell) -> (View UUID)

foreign import inputCellName :: (View InputCell) -> String

foreign import inputCellCode :: (View InputCell) -> String

foreign import viewTree1 :: Json -> (View Tree1)

foreign import unviewTree1 :: (View Tree1) -> Json

foreign import caseTree1 :: forall r. {
  "ArrayTree" :: (View Version1) -> (Array (View Tree1)) -> r,
  "RecordTree" :: (View Version1) -> (Array (View Field1)) -> r,
  "MiscTree" :: (View Version1) -> String -> r
  } -> (View Tree1) -> r

foreign import viewField1 :: Json -> (View Field1)

foreign import unviewField1 :: (View Field1) -> Json

foreign import field1Version :: (View Field1) -> (View Version1)

foreign import field1Key :: (View Field1) -> String

foreign import field1Value :: (View Field1) -> (View Tree1)

foreign import viewDocumentId :: Json -> (View DocumentId)

foreign import unviewDocumentId :: (View DocumentId) -> Json

foreign import unDocumentId :: (View DocumentId) -> Int

foreign import viewResultTree :: Json -> (View ResultTree)

foreign import unviewResultTree :: (View ResultTree) -> Json

foreign import unResultTree :: (View ResultTree) -> (View Tree2)

foreign import viewHash :: Json -> (View Hash)

foreign import unviewHash :: (View Hash) -> Json

foreign import unHash :: (View Hash) -> String

foreign import viewCachedText :: Json -> (View CachedText)

foreign import unviewCachedText :: (View CachedText) -> Json

foreign import caseCachedText :: forall r. {
  "FreshText" :: String -> (View Hash) -> r,
  "CachedText" :: (View Hash) -> r
  } -> (View CachedText) -> r

foreign import viewCachedResult :: Json -> (View CachedResult)

foreign import unviewCachedResult :: (View CachedResult) -> Json

foreign import caseCachedResult :: forall r. {
  "FreshResult" :: (View Result) -> (View Hash) -> r,
  "CachedResult" :: (View Hash) -> r
  } -> (View CachedResult) -> r

