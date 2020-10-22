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
import Effect.Class (class MonadEffect)
import Effect.Class.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell as Cell
import Inflex.Rpc (rpcLoadDocument, rpcRefreshDocument, rpcUpdateDocument)
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
  | NewCell
  | DeleteCell UUID
  | AddField UUID Shared.DataPath String
  | DragStart UUID DE.DragEvent
  | OnDragOver DE.DragEvent
  | OnDrop DE.DragEvent

type State = {
    cells :: Array OutputCell
  , dragUUID :: Maybe UUID
 }

type Input = Unit

type Output = Unit

--------------------------------------------------------------------------------
-- Component

component :: forall q. H.Component HH.HTML q Input Output Aff
component =
  H.mkComponent
    { initialState: const {cells: mempty, dragUUID: Nothing}
    , render
    , eval:
        H.mkEval
          H.defaultEval {initialize = pure Initialize, handleAction = eval}
    }

--------------------------------------------------------------------------------
-- Render

render :: forall state keys m. MonadEffect m =>
   { cells :: Array OutputCell | state }
   -> HH.HTML (H.ComponentSlot HH.HTML ( "Cell" :: H.Slot Cell.Query Cell.Output String | keys) m Command) Command
render state =
  HH.div
    [HP.class_ (HH.ClassName "wrapper")]
    [ HH.div
        [HP.class_ (HH.ClassName "navbar")]
        [ HH.a [HP.class_ (HH.ClassName "logo"), HP.href (meta . dashboard)] []
        , HH.div
            [HP.class_ (HH.ClassName "rhs-nav")]
            [ HH.button
                [ HP.class_ (HH.ClassName "new-cell full-button")
                , HE.onClick (\e -> pure NewCell)
                ]
                [HH.text "New Cell"]
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
                cell
                (\update0 ->
                   pure
                     (case update0 of
                        Cell.CellUpdate update' -> UpdateCell uuid update'
                        Cell.RemoveCell -> DeleteCell uuid
                        Cell.CellDragStart dragEvent -> DragStart uuid dragEvent
                        Cell.CellAddField path string -> AddField uuid path string
                        )))
           (state . cells))
    ]

--------------------------------------------------------------------------------
-- Eval

mediaType :: MediaType
mediaType = (MediaType "text/plain")


eval :: forall t122 t125 t129 t130 t131 t258.
  MonadEffect t129 => MonadAff t129 => Command
                                       -> H.HalogenM
                                            { cells :: Array OutputCell
                                            , dragUUID :: Maybe UUID
                                            | t258
                                            }
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
      result <- rpcLoadDocument (DocumentId (meta.documentId))
      case result of
        Left err -> do
          error ("Error loading document: " <> err) -- TODO:Display this to the user properly.
        Right outputDocument ->
          setOutputDocument outputDocument
    NewCell -> do
      uuid <- H.liftEffect genUUIDV4
      s <- H.get
      let cells' =
            [ InputCell1
                { uuid: uuid
                , name: ""
                , code: ""
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
    AddField uuid path name -> do
      update (Shared.AddFieldUpdate (Shared.NewField {path, name, uuid}))

--------------------------------------------------------------------------------
-- API calls

refresh :: forall t60 t74.
  Bind t60 => MonadEffect t60 => MonadAff t60 => MonadState
                                                   { cells :: Array OutputCell
                                                   | t74
                                                   }
                                                   t60
                                                  => Array InputCell1 -> t60 Unit
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

update :: forall t60 t74.
  Bind t60 => MonadEffect t60 => MonadAff t60 => MonadState
                                                   { cells :: Array OutputCell
                                                   | t74
                                                   }
                                                   t60
                                                  => Shared.Update -> t60 Unit
update update' = do
  result <-
    rpcUpdateDocument
      (Shared.UpdateDocument
         { documentId: DocumentId (meta.documentId)
         , update: update'
         })
  case result of
    Left err -> do
      error err -- TODO:Display this to the user properly.
    Right outputDocument -> setOutputDocument outputDocument

--------------------------------------------------------------------------------
-- Internal state helpers

setOutputDocument :: forall t11 t14.
  MonadState
    { cells :: Array OutputCell
    | t14
    }
    t11
   => OutputDocument -> t11 Unit
setOutputDocument (OutputDocument {cells}) =
  H.modify_ (\s -> s {cells = cells})

toInputCell :: OutputCell -> InputCell1
toInputCell (OutputCell {uuid, name, code, order}) =
  InputCell1 {uuid, name, code, order, version: versionRefl}
