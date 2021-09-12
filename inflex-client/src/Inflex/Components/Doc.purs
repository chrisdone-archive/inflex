-- |

module Inflex.Components.Doc
  ( component
  ) where

import Inflex.Frisson
import Prelude

import Control.Monad.State (class MonadState)
import Data.Array (mapMaybe, concatMap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.MediaType (MediaType(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID(..), uuidToString)
import Dragger as Dragger
import Connector as Connector
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Manage as Manage
import Halogen.Svg as S
import Halogen.VDom.Types as VD
import Inflex.Components.Cell as Cell
import Inflex.Rpc (rpcCsvGuessSchema, rpcCsvImport, rpcGetFiles, rpcLoadDocument, rpcRedoDocument, rpcUndoDocument, rpcUpdateDocument, rpcUpdateSandbox)
import Inflex.Schema (DocumentId(..), InputCell1(..), CachedOutputCell, OutputDocument, versionRefl)
import Inflex.Schema as Shared
import Inflex.Types (OutputCell(..))
import Timed (timed)
import Web.HTML.Event.DragEvent as DE
import Web.UIEvent.MouseEvent as ME

--------------------------------------------------------------------------------
-- Foreign

foreign import meta :: {
  documentId :: Nullable Int,
  logout :: String,
  dashboard :: String,
  readonly :: Boolean,
  loggedin :: Boolean,
  sandbox :: Boolean
 }

foreign import dragEventToMouseEvent :: DE.DragEvent -> ME.MouseEvent

--------------------------------------------------------------------------------
-- Types

data Command
  = Initialize
  -- | UpdateCell UUID {name :: String, code :: String}
  | NewCell String
  | DeleteCell UUID
  | UpdatePath UUID Shared.UpdatePath
  | RenameCell UUID String
  | RepositionCell UUID Int Int
  | Undo
  | Redo
  | ImportCsvStart
  | ChooseCsvFile (View Shared.File)
  | CanvasCreated (Manage.ElemRef Manage.Element)

type State = {
    cells :: Array OutputCell
  , dragUUID :: Maybe UUID
  , modal :: Modal
  , seenTexts :: Map Shared.Hash String
  , seenResults :: Map Shared.Hash (View Shared.Result)
  , dragger :: Maybe Dragger.Dragger
  , loaded :: Boolean
 }

data Modal
  = NoModal
  | ImportCsvModal CsvWizard

data CsvWizard = CsvChooseFile (Array (View Shared.File))

type Input = Unit

type Output = Unit

--------------------------------------------------------------------------------
-- Component

component :: forall q. H.Component HH.HTML q Input Output Aff
component =
  H.mkComponent
    { initialState: const {cells: mempty, dragUUID: Nothing, modal: NoModal, seenResults: mempty, seenTexts: mempty, dragger: Nothing, loaded: false}
    , render: \state -> timed "Doc.render" (\_ -> render state)
    , eval:
        H.mkEval
          H.defaultEval {initialize = pure Initialize, handleAction = eval}
    }

--------------------------------------------------------------------------------
-- Render

render :: forall keys m. MonadAff m =>
   State
   -> HH.HTML (H.ComponentSlot HH.HTML ( "Cell" :: H.Slot Cell.Query Cell.Output String | keys) m Command) Command
render state =
  HH.div
    [ HP.class_
        (HH.ClassName
           ("wrapper" <>
            if meta . sandbox
              then " sandbox"
              else ""))
    ]
    ([ HH.div
         [HP.class_ (HH.ClassName "navbar")]
         [ if meta . sandbox
             then HH.div
                    [HP.class_ (HH.ClassName "sandbox-note")]
                    [HH.text "Try"]
             else HH.text ""
         , HH.a [HP.class_ (HH.ClassName "logo"), HP.href (meta . dashboard)] []
         , HH.div
             [HP.class_ (HH.ClassName "rhs-nav")]
             [ HH.button
                 [ HP.class_ (HH.ClassName "new-cell undo-button full-button")
                 , HE.onClick (\e -> pure Undo)
                 , HP.disabled undoDisabled
                 ]
                 [HH.text "Undo"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell undo-button full-button")
                 , HE.onClick (\e -> pure Redo)
                 , HP.disabled undoDisabled
                 ]
                 [HH.text "Redo"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-prefix full-button")
                 , HP.disabled (meta . readonly)
                 ]
                 [HH.text "New"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\e -> pure (NewCell ""))
                 , HP.disabled (meta . readonly)
                 ]
                 [HH.text "Formula"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure (NewCell "\"\""))
                 , HP.disabled (meta . readonly)
                 ]
                 [HH.text "Text"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure (NewCell "[]"))
                 , HP.disabled (meta . readonly)
                 ]
                 [HH.text "List"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure (NewCell "{}"))
                 , HP.disabled (meta . readonly)
                 ]
                 [HH.text "Record"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure (NewCell "[] :: [{}]"))
                 , HP.disabled (meta . readonly)
                 ]
                 [HH.text "Table"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell import-button full-button")
                 , HE.onClick (\_ -> pure ImportCsvStart)
                 , HP.disabled (meta . readonly)
                 ]
                 [HH.text "Import"]
             , if meta . sandbox
                 then HH.text ""
                 else HH.form
                        [ HP.action (meta . logout)
                        , HP.method HP.POST
                        , if meta . loggedin
                            then HP.class_ (HH.ClassName "")
                            else HP.class_ (HH.ClassName "hidden")
                        ]
                        [ HH.button
                            [HP.class_ (HH.ClassName "logout full-button")]
                            [HH.text "Logout"]
                        ]
             ]
         ]
     , HH.div
         [ HP.class_
             (HH.ClassName
                ("canvas" <>
                 if meta . readonly
                   then " readonly"
                   else ""))
         , Manage.manage CanvasCreated
         ]
         ([svg (state.cells)] <>
          if not (state.loaded) && not (meta.sandbox)
          then
          [HH.div
            [HP.class_ (HH.ClassName "loading-scripts")]
            [HH.div
               [HP.class_ (HH.ClassName "lds-ripple")]
               [HH.div [] []
               ,HH.div [] []]]]
          else
          map
            (\outputCell@(OutputCell cell) ->
               let uuid = (cell . uuid)
                in case state . dragger of
                     Nothing -> HH.text "Pending..."
                     Just dragger ->
                       HH.slot
                         (SProxy :: SProxy "Cell")
                         (uuidToString (cell . uuid))
                         Cell.component
                         (Cell.Input
                            { cell: outputCell
                            , cells:
                                M.delete
                                  uuid
                                  (M.fromFoldable
                                     (map
                                        (\cell'@(OutputCell cell'0) ->
                                           Tuple ((cell'0 . uuid)) cell')
                                        (state . cells)))
                            , dragger
                            })
                         (\update0 ->
                            pure
                              (case update0 of
                                 Cell.RemoveCell -> DeleteCell uuid
                                 Cell.UpdatePath update' ->
                                   UpdatePath uuid update'
                                 Cell.RenameCell name' -> RenameCell uuid name'
                                 Cell.RepositionCell x y -> RepositionCell uuid x y)))
            (state . cells))
     ] <>
     case state . modal of
       NoModal -> []
       ImportCsvModal wizard ->
         [ HH.div
             [HP.class_ (HH.ClassName "modal-wrap")]
             [renderCsvWizard wizard]
         ])
  where
    namesInScope =
      standardNames <>
      M.fromFoldable
        (map
           (\(OutputCell cell) ->
              Tuple (uuidToString (cell . uuid)) (cell . name))
           (state . cells))

standardNames :: Map String String
standardNames =
  M.fromFoldable
  [ Tuple "map" "map"
  , Tuple "filter" "filter"
  , Tuple "concat" "concat"
  , Tuple "sum" "sum"
  , Tuple "average" "average"
  , Tuple "vega" "vega"
  , Tuple "null" "null"
  , Tuple "length" "length"
  , Tuple "distinct" "distinct"
  , Tuple "minimum" "minimum"
  , Tuple "maximum" "maximum"
  , Tuple "sort" "sort"
  , Tuple "find" "find"
  , Tuple "all" "all"
  , Tuple "any" "any"
  ]

renderCsvWizard :: forall t46. CsvWizard -> HH.HTML t46 Command
renderCsvWizard wizard =
  case wizard of
    CsvChooseFile files ->
      HH.div
        [HP.class_ (HH.ClassName "csv-import")]
        [ HH.h1 [] [HH.text "Choose a CSV file that you uploaded"]
        , HH.ul
            [HP.class_ (HH.ClassName "csv-files")]
            (map
               (\file ->
                  HH.li
                    [ HP.class_ (HH.ClassName "csv-file")
                    , HE.onClick (\_ -> pure (ChooseCsvFile file))
                    ]
                    [HH.text (fileName file)])
               files)
        ]

--------------------------------------------------------------------------------
-- Eval

mediaType :: MediaType
mediaType = (MediaType "text/plain")


eval :: forall t122 t125 t129 t130 t131.
  MonadAff t129 => MonadAff t129 => Command
                                       -> H.HalogenM
                                            State
                                            t131
                                            ( "Cell" :: H.Slot Cell.Query t125 String
                                            | t122
                                            )
                                            t130
                                            t129
                                            Unit
eval =
  case _ of
    CanvasCreated elemRef ->
      case elemRef of
        Manage.Created (element) ->
          case Manage.fromElement element of
            Just htmlelement -> do
              connector <- H.liftEffect (Connector.newConnector htmlelement)
              dragger <- H.liftEffect (Dragger.newDragger htmlelement connector)
              H.modify_ (\s -> s {dragger=Just dragger})
            Nothing -> pure unit
        Manage.Removed _ -> pure unit
    Initialize -> do
      case documentId of
        Nothing -> pure unit
        Just docId -> do
          result <- rpcLoadDocument (DocumentId docId)
          case result of
            Left err -> do
              error ("Error loading document:" <> err) -- TODO:Display this to the user properly.
            Right outputDocument -> setOutputDocument outputDocument
    NewCell code -> do
      result <- update (Shared.CellNew (Shared.NewCell {code}))
      case result of
        Nothing -> pure unit
        Just cellError -> pure unit
    UpdatePath uuid update' -> do
      result <-
        update
          (Shared.CellUpdate
             (Shared.UpdateCell {uuid, update: update'}))
      case result of
        Nothing -> pure unit
        Just cellError -> do
          _ <-
            H.query
              (SProxy :: SProxy "Cell")
              (uuidToString uuid)
              (Cell.NestedCellError cellError)
          pure unit
    DeleteCell uuid -> do
      result <- update (Shared.CellDelete (Shared.DeleteCell {uuid}))
      case result of
        Nothing -> pure unit
        Just cellError -> pure unit
    RenameCell uuid name' -> do
      result <-
        update
          (Shared.CellRename
             (Shared.RenameCell {uuid, newname: name'}))
      case result of
        Nothing -> pure unit
        Just cellError -> pure unit
    RepositionCell uuid x y -> do
      result <-
        update
          (Shared.CellReposition
             (Shared.RepositionCell {uuid, x, y}))
      case result of
        Nothing -> pure unit
        Just cellError -> pure unit
    Undo ->
      case documentId of
        Nothing -> error "Sandbox doesn't support undo!"
        Just docId -> do
          {seenTexts,seenResults} <- H.get
          result <- rpcUndoDocument (Shared.TravelDocument{
                    documentId:DocumentId docId, seen: Set.toUnfoldable (M.keys seenResults <> M.keys seenTexts)
          })
          case result of
            Left err -> error err
            Right outputDocument -> setOutputDocument outputDocument
    Redo ->
      case documentId of
        Nothing -> error "Sandbox doesn't support redo!"
        Just docId -> do
          {seenTexts,seenResults} <- H.get
          result <- rpcRedoDocument (Shared.TravelDocument{
                    documentId:DocumentId docId, seen: Set.toUnfoldable (M.keys seenResults <> M.keys seenTexts)
          })
          case result of
            Left err -> error err
            Right outputDocument -> setOutputDocument outputDocument
    ImportCsvStart -> do
      result <- rpcGetFiles (Shared.FileQuery {search: ""})
      case result of
        Left err -> error err
        Right files ->
          H.modify_ (\s -> s {modal = ImportCsvModal (CsvChooseFile (filesOutputFiles files))})
    ChooseCsvFile file ->
      case documentId of
        Nothing -> error "Sandbox doesn't support CSV import!"
        Just docId -> do
          result <- rpcCsvGuessSchema (materializeFile file)
          case result of
            Left err -> error ("rpcCsvGuessSchema:" <> err)
            Right csvGuess ->
              caseCsvGuess {
                "GuessCassavaFailure": \err -> error err,
                "CsvGuessed": \csvImportSpec -> do
                  result2 <-
                    rpcCsvImport
                      (Shared.CsvImportFinal {csvImportSpec: materializeCsvImportSpec csvImportSpec, documentId: DocumentId docId})
                                      -- For now, we're just going to immediately import the
                                      -- file. But next, we'll provide a UI display of the guessed
                                      -- schema, with the option to tweak the types of fields before importing.
                  case result2 of
                    Left err -> error ("CsvImport:" <> err)
                    Right outputDocument -> setOutputDocument outputDocument
              } csvGuess

materializeFile :: View Shared.File -> Shared.File
materializeFile f = Shared.File { id: fileId f, name: fileName f }

materializeCsvImportSpec :: View Shared.CsvImportSpec -> Shared.CsvImportSpec
materializeCsvImportSpec f =
  Shared.CsvImportSpec
    { skipRows: csvImportSpecSkipRows f
    , separator: csvImportSpecSeparator f
    , file: materializeFile (csvImportSpecFile f)
    , columns: map materializeColumn (csvImportSpecColumns f)
    }

materializeColumn :: View Shared.CsvColumn -> Shared.CsvColumn
materializeColumn f =
  Shared.CsvColumn
    { name: csvColumnName f
    , action: materializeAction (csvColumnAction f)
    }

materializeAction :: View Shared.ColumnAction -> Shared.ColumnAction
materializeAction = caseColumnAction {
    "IgnoreColumn": Shared.IgnoreColumn
    , "ImportAction": \col -> Shared.ImportAction (materializeImportColumn col)
  }

materializeImportColumn :: View Shared.ImportColumn -> Shared.ImportColumn
materializeImportColumn f = Shared.ImportColumn {
 importType: materializeColumnType (importColumnImportType f),
 renameTo: importColumnRenameTo f
   }

materializeColumnType :: View Shared.CsvColumnType -> Shared.CsvColumnType
materializeColumnType = caseCsvColumnType {
  "DecimalType": \i o -> Shared.DecimalType i (materializeOptionality o),
  "IntegerType": \o -> Shared.IntegerType  (materializeOptionality o),
  "TextType": \o -> Shared.TextType  (materializeOptionality o)
  }

materializeOptionality :: View Shared.Optionality -> Shared.Optionality
materializeOptionality = caseOptionality {
    "Optional": \v -> Shared.Optional versionRefl,
    "Required": \v -> Shared.Required versionRefl
  }

--------------------------------------------------------------------------------
-- API calls

update :: forall t60.
  Bind t60 => MonadAff t60 => MonadAff t60 => MonadState
                                                   State
                                                   t60
                                                  => Shared.Update -> t60 (Maybe (View Shared.NestedCellError))
update update' =
  case toMaybe (meta . documentId) of
    Nothing -> do
      state <- H.get
      result <-
        rpcUpdateSandbox
          (Shared.UpdateSandbox
             { document: Shared.InputDocument1 {cells: map toInputCell (state.cells)}
             , update: update'
             })
      case result of
        Left err -> do
          error err -- TODO:Display this to the user properly.
          pure Nothing
        Right uresult ->
          caseUpdateResult {
            "UpdatedDocument": \outputDocument -> do setOutputDocument outputDocument
                                                     pure Nothing
            ,"NestedError": \cellError -> pure (Just cellError)
            }
          uresult
    Just docId -> do
      {seenTexts,seenResults} <- H.get
      result <-
        rpcUpdateDocument
          (Shared.UpdateDocument
             { documentId: DocumentId docId
             , update: update'
             , seen: Set.toUnfoldable (M.keys seenResults <> M.keys seenTexts)
             })
      case result of
        Left err -> do
          error err -- TODO:Display this to the user properly.
          pure Nothing
        Right uresult ->
          caseUpdateResult {
            "UpdatedDocument": \outputDocument -> do setOutputDocument outputDocument
                                                     pure Nothing
            ,"NestedError": \cellError -> pure (Just cellError)
            }
          uresult



--------------------------------------------------------------------------------
-- Internal state helpers

setOutputDocument ::
     forall t11. (MonadState State t11)
  => MonadEffect t11 =>
       View OutputDocument -> t11 Unit
setOutputDocument doc = do
  {seenTexts,seenResults} <- H.get
  let cells0 = traverse (consolidateCell seenTexts seenResults) (outputDocumentCells doc)
  case cells0 of
     Left _ -> pure unit
     Right cells ->
       H.modify_
         (\s ->
            s
              { cells = cells
              , loaded = true
              , modal = NoModal
              -- Here:We store the cells we've seen. It's important that
              -- this is the only place where it's updated, as it serves as
              -- a cache.
              , seenTexts =
                  M.fromFoldable -- Collect all hashes seen,
                                   -- discarding the type of thing the
                                   -- hash refers to. TODO: Don't do that?
                    (mapMaybe
                       (\outputCell ->
                              caseCachedText {
                                 "FreshText": \text hash -> pure (Tuple (materializeHash hash) text)
                                , "CachedText": \hash ->
                                      map (Tuple (materializeHash hash)) (M.lookup (materializeHash hash) seenTexts)
                               } (cachedOutputCellCode outputCell)

                                       )
                       (outputDocumentCells doc))
               , seenResults =
                  M.fromFoldable
                     (mapMaybe
                       (\outputCell ->
                              caseCachedResult {
                                 "FreshResult": \result hash -> pure (Tuple (materializeHash hash) result)
                                , "CachedResult": \hash ->
                                  map (Tuple (materializeHash hash)) (M.lookup (materializeHash hash) seenResults)
                               } (cachedOutputCellResult outputCell)

                                       )
                       (outputDocumentCells doc))
              })

materializeHash :: View Shared.Hash -> Shared.Hash
materializeHash x = Shared.Hash (unHash x)

consolidateCell
  :: Map Shared.Hash String
  -> Map Shared.Hash (View Shared.Result)
  -> View CachedOutputCell
  -> Either String OutputCell
consolidateCell seenTexts seenResults cached = do
  {code,codeHash} <- caseCachedText {
          "FreshText": \text hash -> pure {code: text, codeHash: materializeHash hash}
         , "CachedText": \hash -> case M.lookup (materializeHash hash) seenTexts of
              Just v -> Right {code: v, codeHash: materializeHash hash}
              Nothing -> Left "key not found"
        } (cachedOutputCellCode cached)
  {result,resultHash} <- caseCachedResult {
          "FreshResult": \result hash -> pure {result, resultHash: materializeHash hash}
         , "CachedResult": \hash -> case M.lookup (materializeHash hash) seenResults of
              Just v -> Right {result: v, resultHash: materializeHash hash}
              Nothing -> Left "key not found"
        } (cachedOutputCellResult cached)
  pure (OutputCell
    { uuid: UUID (unUUID (cachedOutputCellUuid cached))
    , name: cachedOutputCellName cached
    , order: cachedOutputCellOrder cached
    , code
    , codeHash
    , result
    , resultHash
    , position: cachedOutputCellPosition cached
    , dependencies: map (\uuid -> UUID (unUUID uuid)) (cachedOutputCellDependencies cached)
    })

toInputCell :: OutputCell -> InputCell1
toInputCell (OutputCell cell) =
  InputCell1
    { uuid: (cell.uuid)
    , name: cell.name
    , code: cell.code
    , order: cell.order
    , version: versionRefl
    , position: casePosition {
          "Unpositioned": Shared.Unpositioned,
          "AbsolutePosition": Shared.AbsolutePosition
         } (cell.position)
    }

documentId :: Maybe Int
documentId = toMaybe (meta . documentId)

undoDisabled :: Boolean
undoDisabled = meta.readonly || isNothing documentId

--------------------------------------------------------------------------------
-- SVG

svg cells =
  S.elem
    "svg"
    [S.attr "width" "100%", S.attr "height" "100%"]
    ([ S.elem
         "defs"
         []
         [ S.elem
             "marker"
             [ S.attr "id" "arrowhead"
             , S.attr "markerWidth" "10"
             , S.attr "markerHeight" "10"
             , S.attr "refX" "10"
             , S.attr "refY" "3.5"
             , S.attr "orient" "auto"
             , S.attr "fill" "#959da7"
             ]
             [S.elem "polygon" [S.attr "points" "0 0, 10 3.5, 0 7"] []]
         ]
     ] <>
     concatMap
       (\(OutputCell cell) ->
          map
            (\dep ->
               S.elem
                 "line"
                 [ S.attr "x1" "0"
                 , S.attr "y1" "0"
                 , S.attr "x2" "0"
                 , S.attr "y2" "0"
                 , S.attr "stroke" "#959da7"
                 , S.attr "stroke-width" "2"
                 , S.attr "marker-end" "url(#arrowhead)"
                 , S.attr "data-from_id" ("cell-" <> uuidToString dep)
                 , S.attr "data-to_id" ("cell-" <> uuidToString (cell . uuid))
                 , S.attr "class" "connector-arrow"
                 ]
                 [])
            (cell . dependencies))
       cells)
