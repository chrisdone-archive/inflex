-- |

module Inflex.Components.Doc
  ( component
  ) where

import Control.Monad.State (class MonadState)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, genUUIDV4, uuidToString)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell as Cell
import Inflex.Rpc (rpcCsvGuessSchema, rpcCsvImport, rpcGetFiles, rpcLoadDocument, rpcRedoDocument, rpcRefreshDocument, rpcUndoDocument, rpcUpdateDocument)
import Inflex.Schema (DocumentId(..), InputCell1(..), InputDocument1(..), OutputCell(..), OutputDocument(..), RefreshDocument(..), versionRefl)
import Inflex.Schema as Shared
import Prelude (class Bind, Unit, bind, const, discard, map, mempty, pure, unit, (+), (/=), (<<<), (<>), (==))
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent as DE
import Web.UIEvent.MouseEvent as ME

--------------------------------------------------------------------------------
-- Foreign

foreign import meta :: { documentId :: Int, logout :: String, dashboard :: String }

foreign import dragEventToMouseEvent :: DE.DragEvent -> ME.MouseEvent

--------------------------------------------------------------------------------
-- Types

data Command
  = Initialize
  | UpdateCell UUID {name :: String, code :: String}
  | NewCell String
  | DeleteCell UUID
  | UpdatePath UUID Shared.UpdatePath
  | DragStart UUID DE.DragEvent
  | OnDragOver DE.DragEvent
  | OnDrop DE.DragEvent
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
    , render
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
    [HP.class_ (HH.ClassName "wrapper")]
    ([ HH.div
         [HP.class_ (HH.ClassName "navbar")]
         [ HH.a [HP.class_ (HH.ClassName "logo"), HP.href (meta . dashboard)] []
         , HH.div
             [HP.class_ (HH.ClassName "rhs-nav")]
             [ HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\e -> pure Undo)
                 ]
                 [HH.text "Undo"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\e -> pure Redo)
                 ]
                 [HH.text "Redo"]
             , HH.button
                 [HP.class_ (HH.ClassName "new-prefix full-button")]
                 [HH.text "New"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\e -> pure (NewCell ""))
                 ]
                 [HH.text "Formula"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure (NewCell "\"\""))
                 ]
                 [HH.text "Text"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure (NewCell "[]"))
                 ]
                 [HH.text "List"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure (NewCell "[] :: [{}]"))
                 ]
                 [HH.text "Table"]
             , HH.button
                 [ HP.class_ (HH.ClassName "new-cell full-button")
                 , HE.onClick (\_ -> pure ImportCsvStart)
                 ]
                 [HH.text "Import"]
             , HH.form
                 [HP.action (meta . logout), HP.method HP.POST]
                 [ HH.button
                     [HP.class_ (HH.ClassName "logout full-button")]
                     [HH.text "Logout"]
                 ]
             ]
         ]
     , HH.div
         [ HP.class_ (HH.ClassName "canvas")
         , HE.onDragOver (Just <<< OnDragOver)
         , HE.onDrop (Just <<< OnDrop)
         ]
         (map
            (\cell@(OutputCell {uuid}) ->
               HH.slot
                 (SProxy :: SProxy "Cell")
                 (uuidToString uuid)
                 Cell.component
                 (Cell.Input {cell, namesInScope})
                 (\update0 ->
                    pure
                      (case update0 of
                         Cell.CellUpdate update' -> UpdateCell uuid update'
                         Cell.RemoveCell -> DeleteCell uuid
                         Cell.CellDragStart dragEvent ->
                           DragStart uuid dragEvent
                         Cell.UpdatePath update' -> UpdatePath uuid update')))
            (state . cells))
     ] <>
     case state . modal of
       NoModal -> []
       ImportCsvModal wizard ->
         [ HH.div
             [HP.class_ (HH.ClassName "modal-wrap")]
             [renderCsvWizard wizard]
         ])
  where namesInScope = standardNames <>
                       map (\(OutputCell {name}) -> name) (state.cells)

standardNames :: Array String
standardNames =
  [ "map"
  , "filter"
  , "sum"
  , "average"
  , "vega"
  , "null"
  , "length"
  , "distinct"
  , "minimum"
  , "maximum"
  , "sort"
  , "find"
  , "all"
  , "any"
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
    OnDrop dragEvent -> do
      pure unit
      H.liftEffect (preventDefault (DE.toEvent dragEvent)) -- To prevent navigating to thing?
    DragStart uuid dragEvent -> do
      H.modify_ (\s -> s {dragUUID = Just uuid})
    OnDragOver dragEvent -> do
      H.liftEffect (preventDefault (DE.toEvent dragEvent)) -- To prevent animation?
      muuid <- H.gets (_ . dragUUID)
      case muuid of
        Nothing -> pure unit
        Just uuid -> do
          let x = ME.clientX (dragEventToMouseEvent dragEvent)
              y = ME.clientY (dragEventToMouseEvent dragEvent)
          _ <-
            H.query
              (SProxy :: SProxy "Cell")
              (uuidToString uuid)
              (Cell.SetXY {x, y})
          pure unit
    Initialize -> do
      result <- rpcLoadDocument (DocumentId (meta . documentId))
      case result of
        Left err -> do
          error ("Error loading document:" <> err) -- TODO:Display this to the user properly.
        Right outputDocument -> setOutputDocument outputDocument
    NewCell code -> do
      uuid <- H.liftEffect genUUIDV4
      s <- H.get
      let cells' =
            [ InputCell1
                { uuid: uuid
                , name: ""
                , code
                , order:
                    fromMaybe
                      0
                      (maximum
                         (map (\(OutputCell {order}) -> order) (s . cells))) +
                    1
                , version: versionRefl
                }
            ] <>
            map toInputCell (s . cells)
      refresh cells'
    UpdateCell uuid cell -> do
      state <- H.get
      refresh
        (map
           (\original@(InputCell1 {uuid: uuid', order, version}) ->
              if uuid' == uuid
                then InputCell1
                       { uuid
                       , code: cell . code
                       , name: cell . name
                       , order
                       , version
                       }
                else original)
           (map toInputCell (state . cells)))
    DeleteCell uuid -> do
      state <- H.get
      refresh
        (map
           toInputCell
           (filter
              (\(OutputCell {uuid: uuid'}) -> uuid' /= uuid)
              (state . cells)))
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
    Undo -> do
      result <- rpcUndoDocument (DocumentId (meta . documentId))
      case result of
        Left err -> error err
        Right outputDocument -> setOutputDocument outputDocument
    Redo -> do
      result <- rpcRedoDocument (DocumentId (meta . documentId))
      case result of
        Left err -> error err
        Right outputDocument -> setOutputDocument outputDocument
    ImportCsvStart -> do
      result <- rpcGetFiles (Shared.FileQuery {search: ""})
      case result of
        Left err -> error err
        Right (Shared.FilesOutput {files}) ->
          H.modify_ (\s -> s {modal = ImportCsvModal (CsvChooseFile files)})
    ChooseCsvFile file -> do
      result <- rpcCsvGuessSchema file
      case result of
        Left err -> error ("rpcCsvGuessSchema: " <> err)
        Right csvGuess ->
          case csvGuess of
            Shared.GuessCassavaFailure err -> error err
            Shared.CsvGuessed csvImportSpec -> do
              result2 <-
                rpcCsvImport (Shared.CsvImportFinal {csvImportSpec, documentId})
              -- For now, we're just going to immediately import the
              -- file. But next, we'll provide a UI display of the guessed
              -- schema, with the option to tweak the types of fields before importing.
              case result2 of
                Left err -> error ("CsvImport: " <> err)
                Right outputDocument -> setOutputDocument outputDocument
  where
    documentId = DocumentId (meta . documentId)

--------------------------------------------------------------------------------
-- API calls

refresh ::
     forall t60. Bind t60
  => MonadAff t60 =>
       MonadAff t60 =>
         MonadState State t60 =>
           Array InputCell1 -> t60 Unit
refresh cells = do
  result <-
    rpcRefreshDocument
      (RefreshDocument
         { documentId: DocumentId (meta.documentId)
         , document: InputDocument1 {cells: cells}
         })
  case result of
    Left err -> do
      error err -- TODO:Display this to the user properly.
    Right outputDocument -> setOutputDocument outputDocument

update :: forall t60.
  Bind t60 => MonadAff t60 => MonadAff t60 => MonadState
                                                   State
                                                   t60
                                                  => Shared.Update -> t60 (Maybe Shared.NestedCellError)
update update' = do
  result <-
    rpcUpdateDocument
      (Shared.UpdateDocument
         { documentId: DocumentId (meta . documentId)
         , update: update'
         })
  case result of
    Left err -> do
      error err -- TODO:Display this to the user properly.
      pure Nothing
    Right uresult ->
      case uresult of
        Shared.UpdatedDocument outputDocument -> do
          setOutputDocument outputDocument
          pure Nothing
        Shared.NestedError cellError -> do
          pure (Just cellError)

--------------------------------------------------------------------------------
-- Internal state helpers

setOutputDocument ::
     forall t11. MonadState State t11
  => OutputDocument
  -> t11 Unit
setOutputDocument (OutputDocument {cells}) =
  H.modify_ (\s -> s {cells = cells, modal = NoModal})

toInputCell :: OutputCell -> InputCell1
toInputCell (OutputCell {uuid, name, code, order}) =
  InputCell1 {uuid, name, code, order, version: versionRefl}
