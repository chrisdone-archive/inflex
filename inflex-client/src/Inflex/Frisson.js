exports.unsafeView = function(x){return x}

exports.showView = function(x){return JSON.stringify(x)}
exports.viewUUID = function(x){return x};

exports.unviewUUID = function(x){return x};

exports.unUUID = function(x){return x};

exports.viewRefreshDocument = function(x){return x};

exports.unviewRefreshDocument = function(x){return x};

exports.refreshDocumentDocument = function(a){return a[0]};

exports.refreshDocumentDocumentId = function(a){return a[1]};

exports.viewUpdateDocument = function(x){return x};

exports.unviewUpdateDocument = function(x){return x};

exports.updateDocumentDocumentId = function(a){return a[0]};

exports.updateDocumentUpdate = function(a){return a[1]};

exports.updateDocumentSeen = function(a){return a[2]};

exports.viewUpdateSandbox = function(x){return x};

exports.unviewUpdateSandbox = function(x){return x};

exports.updateSandboxUpdate = function(a){return a[0]};

exports.updateSandboxDocument = function(a){return a[1]};

exports.viewUpdateResult = function(x){return x};

exports.unviewUpdateResult = function(x){return x};

exports.caseUpdateResult = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["UpdatedDocument"](a[1]);
case 1: return k["NestedError"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewNestedCellError = function(x){return x};

exports.unviewNestedCellError = function(x){return x};

exports.nestedCellErrorPath = function(a){return a[0]};

exports.nestedCellErrorError = function(a){return a[1]};

exports.viewUpdate = function(x){return x};

exports.unviewUpdate = function(x){return x};

exports.caseUpdate = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["CellUpdate"](a[1]);
case 1: return k["CellRename"](a[1]);
case 2: return k["CellDelete"](a[1]);
case 3: return k["CellNew"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewNewCell = function(x){return x};

exports.unviewNewCell = function(x){return x};

exports.newCellCode = function(x){return x};

exports.viewDeleteCell = function(x){return x};

exports.unviewDeleteCell = function(x){return x};

exports.deleteCellUuid = function(x){return x};

exports.viewRenameCell = function(x){return x};

exports.unviewRenameCell = function(x){return x};

exports.renameCellUuid = function(a){return a[0]};

exports.renameCellNewname = function(a){return a[1]};

exports.viewUpdateCell = function(x){return x};

exports.unviewUpdateCell = function(x){return x};

exports.updateCellUuid = function(a){return a[0]};

exports.updateCellUpdate = function(a){return a[1]};

exports.viewUpdatePath = function(x){return x};

exports.unviewUpdatePath = function(x){return x};

exports.updatePathPath = function(a){return a[0]};

exports.updatePathUpdate = function(a){return a[1]};

exports.viewPathUpdate = function(x){return x};

exports.unviewPathUpdate = function(x){return x};

exports.casePathUpdate = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["NewFieldUpdate"](a[1]);
case 1: return k["RenameFieldUpdate"](a[1]);
case 2: return k["DeleteFieldUpdate"](a[1]);
case 3: return k["RemoveUpdate"](a[1]);
case 4: return k["AddToEndUpdate"];
case 5: return k["CodeUpdate"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewCode = function(x){return x};

exports.unviewCode = function(x){return x};

exports.codeText = function(x){return x};

exports.viewRemoval = function(x){return x};

exports.unviewRemoval = function(x){return x};

exports.removalIndex = function(x){return x};

exports.viewNewField = function(x){return x};

exports.unviewNewField = function(x){return x};

exports.newFieldName = function(x){return x};

exports.viewRenameField = function(x){return x};

exports.unviewRenameField = function(x){return x};

exports.renameFieldFrom = function(a){return a[0]};

exports.renameFieldTo = function(a){return a[1]};

exports.viewDeleteField = function(x){return x};

exports.unviewDeleteField = function(x){return x};

exports.deleteFieldName = function(x){return x};

exports.viewDataPath = function(x){return x};

exports.unviewDataPath = function(x){return x};

exports.caseDataPath = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["DataHere"];
case 1: return k["DataElemOf"](a[1])(a[2]);
case 2: return k["DataFieldOf"](a[1])(a[2]);
case 3: return k["DataVariantOf"](a[1])(a[2]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewOutputDocument = function(x){return x};

exports.unviewOutputDocument = function(x){return x};

exports.outputDocumentCells = function(x){return x};

exports.viewInputDocument1 = function(x){return x};

exports.unviewInputDocument1 = function(x){return x};

exports.inputDocument1Cells = function(x){return x};

exports.viewCachedOutputCell = function(x){return x};

exports.unviewCachedOutputCell = function(x){return x};

exports.cachedOutputCellUuid = function(a){return a[0]};

exports.cachedOutputCellName = function(a){return a[1]};

exports.cachedOutputCellCode = function(a){return a[2]};

exports.cachedOutputCellResult = function(a){return a[3]};

exports.cachedOutputCellOrder = function(a){return a[4]};

exports.viewInputCell1 = function(x){return x};

exports.unviewInputCell1 = function(x){return x};

exports.inputCell1Uuid = function(a){return a[0]};

exports.inputCell1Name = function(a){return a[1]};

exports.inputCell1Code = function(a){return a[2]};

exports.inputCell1Order = function(a){return a[3]};

exports.inputCell1Version = function(a){return a[4]};

exports.viewResult = function(x){return x};

exports.unviewResult = function(x){return x};

exports.caseResult = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["ResultError"](a[1]);
case 1: return k["ResultOk"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewTree2 = function(x){return x};

exports.unviewTree2 = function(x){return x};

exports.caseTree2 = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["ArrayTree2"](a[1])(a[2])(a[3]);
case 1: return k["RecordTree2"](a[1])(a[2])(a[3]);
case 2: return k["TextTree2"](a[1])(a[2])(a[3]);
case 3: return k["VegaTree2"](a[1])(a[2])(a[3]);
case 4: return k["VariantTree2"](a[1])(a[2])(a[3])(a[4]);
case 5: return k["MiscTree2"](a[1])(a[2])(a[3]);
case 6: return k["TableTreeMaybe2"](a[1])(a[2])(a[3])(a[4]);
case 7: return k["HoleTree"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewVariantArgument = function(x){return x};

exports.unviewVariantArgument = function(x){return x};

exports.caseVariantArgument = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["VariantArgument"](a[1]);
case 1: return k["NoVariantArgument"];
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewMaybeRow = function(x){return x};

exports.unviewMaybeRow = function(x){return x};

exports.caseMaybeRow = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["SomeRow"](a[1]);
case 1: return k["HoleRow"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewRow = function(x){return x};

exports.unviewRow = function(x){return x};

exports.rowSource = function(a){return a[0]};

exports.rowFields = function(a){return a[1]};

exports.viewField2 = function(x){return x};

exports.unviewField2 = function(x){return x};

exports.field2Version = function(a){return a[0]};

exports.field2Key = function(a){return a[1]};

exports.field2Value = function(a){return a[2]};

exports.viewOriginalSource = function(x){return x};

exports.unviewOriginalSource = function(x){return x};

exports.caseOriginalSource = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["OriginalSource"](a[1]);
case 1: return k["NoOriginalSource"];
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewCellError = function(x){return x};

exports.unviewCellError = function(x){return x};

exports.caseCellError = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["SyntaxError"];
case 1: return k["FillErrors"](a[1]);
case 2: return k["CyclicCells"](a[1]);
case 3: return k["DuplicateCellName"];
case 4: return k["CellRenameErrors"];
case 5: return k["CellTypeError"];
case 6: return k["CellStepEror"];
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewFillError = function(x){return x};

exports.unviewFillError = function(x){return x};

exports.caseFillError = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["NoSuchGlobal"](a[1]);
case 1: return k["OtherCellProblem"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewFileQuery = function(x){return x};

exports.unviewFileQuery = function(x){return x};

exports.fileQuerySearch = function(x){return x};

exports.viewFilesOutput = function(x){return x};

exports.unviewFilesOutput = function(x){return x};

exports.filesOutputFiles = function(x){return x};

exports.viewFile = function(x){return x};

exports.unviewFile = function(x){return x};

exports.fileId = function(a){return a[0]};

exports.fileName = function(a){return a[1]};

exports.viewCsvCheckStatus = function(x){return x};

exports.unviewCsvCheckStatus = function(x){return x};

exports.caseCsvCheckStatus = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["CsvParsesHappily"];
case 1: return k["CsvColumnFailures"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewCsvImportFinal = function(x){return x};

exports.unviewCsvImportFinal = function(x){return x};

exports.csvImportFinalCsvImportSpec = function(a){return a[0]};

exports.csvImportFinalDocumentId = function(a){return a[1]};

exports.viewCsvColumnProblem = function(x){return x};

exports.unviewCsvColumnProblem = function(x){return x};

exports.caseCsvColumnProblem = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["FoundNonInteger"](a[1]);
case 1: return k["FoundNonDecimal"](a[1]);
case 2: return k["RequiredColumnHasEmpty"];
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewCsvGuess = function(x){return x};

exports.unviewCsvGuess = function(x){return x};

exports.caseCsvGuess = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["CsvGuessed"](a[1]);
case 1: return k["GuessCassavaFailure"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewCsvImportSpec = function(x){return x};

exports.unviewCsvImportSpec = function(x){return x};

exports.csvImportSpecFile = function(a){return a[0]};

exports.csvImportSpecSkipRows = function(a){return a[1]};

exports.csvImportSpecSeparator = function(a){return a[2]};

exports.csvImportSpecColumns = function(a){return a[3]};

exports.viewCsvColumn = function(x){return x};

exports.unviewCsvColumn = function(x){return x};

exports.csvColumnName = function(a){return a[0]};

exports.csvColumnAction = function(a){return a[1]};

exports.viewColumnAction = function(x){return x};

exports.unviewColumnAction = function(x){return x};

exports.caseColumnAction = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["IgnoreColumn"];
case 1: return k["ImportAction"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewImportColumn = function(x){return x};

exports.unviewImportColumn = function(x){return x};

exports.importColumnImportType = function(a){return a[0]};

exports.importColumnRenameTo = function(a){return a[1]};

exports.viewCsvColumnType = function(x){return x};

exports.unviewCsvColumnType = function(x){return x};

exports.caseCsvColumnType = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["IntegerType"](a[1]);
case 1: return k["DecimalType"](a[1])(a[2]);
case 2: return k["TextType"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewOptionality = function(x){return x};

exports.unviewOptionality = function(x){return x};

exports.caseOptionality = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["Optional"](a[1]);
case 1: return k["Required"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewInputDocument = function(x){return x};

exports.unviewInputDocument = function(x){return x};

exports.inputDocumentCells = function(x){return x};

exports.viewInputCell = function(x){return x};

exports.unviewInputCell = function(x){return x};

exports.inputCellUuid = function(a){return a[0]};

exports.inputCellName = function(a){return a[1]};

exports.inputCellCode = function(a){return a[2]};

exports.viewTree1 = function(x){return x};

exports.unviewTree1 = function(x){return x};

exports.caseTree1 = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["ArrayTree"](a[1])(a[2]);
case 1: return k["RecordTree"](a[1])(a[2]);
case 2: return k["MiscTree"](a[1])(a[2]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewField1 = function(x){return x};

exports.unviewField1 = function(x){return x};

exports.field1Version = function(a){return a[0]};

exports.field1Key = function(a){return a[1]};

exports.field1Value = function(a){return a[2]};

exports.viewDocumentId = function(x){return x};

exports.unviewDocumentId = function(x){return x};

exports.unDocumentId = function(x){return x};

exports.viewResultTree = function(x){return x};

exports.unviewResultTree = function(x){return x};

exports.resultTreeTree = function(a){return a[0]};

exports.resultTreeTyp = function(a){return a[1]};

exports.viewHash = function(x){return x};

exports.unviewHash = function(x){return x};

exports.unHash = function(x){return x};

exports.viewCachedText = function(x){return x};

exports.unviewCachedText = function(x){return x};

exports.caseCachedText = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["FreshText"](a[1])(a[2]);
case 1: return k["CachedText"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewCachedResult = function(x){return x};

exports.unviewCachedResult = function(x){return x};

exports.caseCachedResult = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["FreshResult"](a[1])(a[2]);
case 1: return k["CachedResult"](a[1]);
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewTypeOf = function(x){return x};

exports.unviewTypeOf = function(x){return x};

exports.caseTypeOf = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["ArrayOf"](a[1]);
case 1: return k["RecordOf"](a[1]);
case 2: return k["TableOf"](a[1]);
case 3: return k["VariantOf"](a[1])(a[2]);
case 4: return k["MiscType"];
default: throw Exception('BUG: case accessor failed');
}
}
}
;

exports.viewNamedType = function(x){return x};

exports.unviewNamedType = function(x){return x};

exports.namedTypeName = function(a){return a[0]};

exports.namedTypeTyp = function(a){return a[1]};

exports.viewOpenClosed = function(x){return x};

exports.unviewOpenClosed = function(x){return x};

exports.caseOpenClosed = function(k) {
return function(a) {
switch (a[0]) {
case 0: return k["Open"];
case 1: return k["Closed"];
default: throw Exception('BUG: case accessor failed');
}
}
}
;

