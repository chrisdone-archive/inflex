-- |

module Inflex.Components.Doc
  ( component
  ) where

import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.MediaType (MediaType(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, uuidToString)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell as Cell
import Inflex.Frisson
import Inflex.Rpc (rpcCsvGuessSchema, rpcCsvImport, rpcGetFiles, rpcLoadDocument, rpcRedoDocument, rpcUndoDocument, rpcUpdateDocument, rpcUpdateSandbox)
import Inflex.Schema (DocumentId(..), InputCell1(..), OutputCell(..), OutputDocument(..), versionRefl)
import Inflex.Schema as Shared
import Prelude
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
  | Undo
  | Redo
  | ImportCsvStart
  | ChooseCsvFile Shared.File

type State = {
    cells :: Array OutputCell
  , dragUUID :: Maybe UUID
  , modal :: Modal
 }

data Modal
  = NoModal
  | ImportCsvModal CsvWizard

data CsvWizard = CsvChooseFile (Array Shared.File)

type Input = Unit

type Output = Unit

--------------------------------------------------------------------------------
-- Component

component :: forall q. H.Component HH.HTML q Input Output Aff
component =
  H.mkComponent
    { initialState: const {cells: mempty, dragUUID: Nothing, modal: NoModal}
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
         ]
         (map
            (\cell@(OutputCell {uuid, name}) ->
               HH.slot
                 (SProxy :: SProxy "Cell")
                 (uuidToString uuid)
                 Cell.component
                 (Cell.Input
                    { cell
                    , cells:
                        M.delete
                          uuid
                          (M.fromFoldable
                             (map
                                (\cell'@(OutputCell {uuid: uuid'}) -> Tuple uuid' cell')
                                (state . cells)))
                    })
                 (\update0 ->
                    pure
                      (case update0 of
                         Cell.RemoveCell -> DeleteCell uuid
                         Cell.UpdatePath update' -> UpdatePath uuid update'
                         Cell.RenameCell name' -> RenameCell uuid name')))
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
           (\(OutputCell {uuid, name}) -> Tuple (uuidToString uuid) name)
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
               (\file@(Shared.File {name}) ->
                  HH.li
                    [ HP.class_ (HH.ClassName "csv-file")
                    , HE.onClick (\_ -> pure (ChooseCsvFile file))
                    ]
                    [HH.text name])
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
    Initialize ->
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
    Undo ->
      case documentId of
        Nothing -> error "Sandbox doesn't support undo!"
        Just docId -> do
          result <- rpcUndoDocument (DocumentId docId)
          case result of
            Left err -> error err
            Right outputDocument -> setOutputDocument outputDocument
    Redo ->
      case documentId of
        Nothing -> error "Sandbox doesn't support redo!"
        Just docId -> do
          result <- rpcRedoDocument (DocumentId docId)
          case result of
            Left err -> error err
            Right outputDocument -> setOutputDocument outputDocument
    ImportCsvStart -> do
      result <- rpcGetFiles (Shared.FileQuery {search: ""})
      pure unit
      {-case result of
        Left err -> error err
        Right (Shared.FilesOutput {files}) ->
          H.modify_ (\s -> s {modal = ImportCsvModal (CsvChooseFile files)})-}
    ChooseCsvFile file ->
      case documentId of
        Nothing -> error "Sandbox doesn't support CSV import!"
        Just docId -> do
          result <- rpcCsvGuessSchema file
          case result of
            Left err -> error ("rpcCsvGuessSchema:" <> err)
            Right csvGuess ->
              pure unit
              {-case csvGuess of
                Shared.GuessCassavaFailure err -> error err
                Shared.CsvGuessed csvImportSpec -> do
                  result2 <-
                    rpcCsvImport
                      (Shared.CsvImportFinal {csvImportSpec, documentId: DocumentId docId})
                                      -- For now, we're just going to immediately import the
                                      -- file. But next, we'll provide a UI display of the guessed
                                      -- schema, with the option to tweak the types of fields before importing.
                  case result2 of
                    Left err -> error ("CsvImport:" <> err)
                    Right outputDocument -> setOutputDocument outputDocument-}

--------------------------------------------------------------------------------
-- API calls

update :: forall t60.
  Bind t60 => MonadAff t60 => MonadAff t60 => MonadState
                                                   State
                                                   t60
                                                  => Shared.Update -> t60 (Maybe Shared.NestedCellError)
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
      pure Nothing
      {-case result of
        Left err -> do
          error err -- TODO:Display this to the user properly.
          pure Nothing
        Right uresult ->
          case uresult of
            Shared.UpdatedDocument outputDocument -> do
              setOutputDocument outputDocument
              pure Nothing
            Shared.NestedError cellError -> do
              pure (Just cellError)-}
    Just docId -> do
      result <-
        rpcUpdateDocument
          (Shared.UpdateDocument
             { documentId: DocumentId docId
             , update: update'
             })
      case result of
        Left err -> do
          error err -- TODO:Display this to the user properly.
          pure Nothing
        Right uresult ->
          pure Nothing
          {-case uresult of
            Shared.UpdatedDocument outputDocument -> do
              setOutputDocument outputDocument
              pure Nothing
            Shared.NestedError cellError -> do
              pure (Just cellError)-}

--------------------------------------------------------------------------------
-- Internal state helpers

setOutputDocument ::
     forall t11. MonadState State t11
  => View OutputDocument
  -> t11 Unit
setOutputDocument _ {-(OutputDocument {cells})-} =
  H.modify_ (\s -> s {cells = s.cells {-cells-}, modal = NoModal})

toInputCell :: OutputCell -> InputCell1
toInputCell (OutputCell {uuid, name, code, order}) =
  InputCell1 {uuid, name, code, order, version: versionRefl}

documentId :: Maybe Int
documentId = toMaybe (meta . documentId)

undoDisabled :: Boolean
undoDisabled = meta.readonly || isNothing documentId
