-- |

module Inflex.Components.Doc
  ( component
  ) where

import Control.Monad.State (class MonadState)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, genUUIDV4, uuidToString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell as Cell
import Inflex.Schema (DocumentId(..), InputCell(..), InputDocument(..), OutputCell(..), OutputDocument(..), RefreshDocument(..))
import Inflex.Rpc (rpcLoadDocument, rpcRefreshDocument)
import Prelude

--------------------------------------------------------------------------------
-- Foreign

foreign import getDocumentId :: Effect Int

--------------------------------------------------------------------------------
-- Types

data Command
  = Initialize
  | UpdateCell UUID {name :: String, code :: String}
  | NewCell
  | DeleteCell UUID

type State = {
  cells :: Array OutputCell
 }

type Input = Unit

type Output = Unit

--------------------------------------------------------------------------------
-- Component

component :: forall q. H.Component HH.HTML q Input Output Aff
component =
  H.mkComponent
    { initialState: const {cells: mempty}
    , render
    , eval:
        H.mkEval
          H.defaultEval {initialize = pure Initialize, handleAction = eval}
    }

--------------------------------------------------------------------------------
-- Render

render :: forall q state keys m. MonadEffect m =>
   { cells :: Array OutputCell | state }
   -> HH.HTML (H.ComponentSlot HH.HTML ( "Cell" :: H.Slot q Cell.Output String | keys) m Command) Command
render state =
  HH.div
    [HP.class_ (HH.ClassName "container-fluid")]
    [ HH.div
        [HP.class_ (HH.ClassName "row")]
        [ HH.button
            [ HP.class_ (HH.ClassName "mt-3 ml-3 mr-3 btn-primary btn")
            , HE.onClick (\e -> pure NewCell)
            ]
            [HH.text "New Cell"]
        ]
    , HH.div
        [HP.class_ (HH.ClassName "row")]
        (map
           (\cell@(OutputCell {uuid}) ->
              HH.slot
                (SProxy :: SProxy "Cell")
                (uuidToString uuid)
                Cell.component
                cell
                (\update ->
                   pure
                     (case update of
                        Cell.CellUpdate update' -> UpdateCell uuid update'
                        Cell.RemoveCell -> DeleteCell uuid)))
           (state . cells))
    ]

--------------------------------------------------------------------------------
-- Eval

eval :: forall m. MonadState State m => MonadEffect m => MonadAff m => Command -> m Unit
eval =
  case _ of
    Initialize -> do
      documentId <- H.liftEffect getDocumentId
      log "Loading document ..."
      result <- rpcLoadDocument (DocumentId documentId)
      case result of
        Left err -> do
          error err -- TODO:Display this to the user properly.
        Right outputDocument -> setOutputDocument outputDocument
    NewCell -> do
      uuid <- H.liftEffect genUUIDV4
      s <- H.get
      let cells' =
            [ InputCell
                { uuid: uuid
                , name: "x"
                , code: ""
                }
            ] <>
            map toInputCell (s . cells)
      H.liftEffect (log "New cell, refreshing ...")
      refresh cells'
    UpdateCell uuid cell -> do
      state <- H.get
      H.liftEffect (log "Cell updated, refreshing ...")
      refresh
        (map
           (\original@(InputCell {uuid: uuid'}) ->
              if uuid' == uuid
                then InputCell
                       { uuid
                       , code: cell . code
                       , name: cell . name
                       }
                else original)
           (map toInputCell (state . cells)))
    DeleteCell uuid -> do
      state <- H.get
      H.liftEffect (log "Cell deleted, refreshing ...")
      documentId <- H.liftEffect getDocumentId
      refresh
        (map
           toInputCell
           (filter
              (\(OutputCell {uuid: uuid'}) -> uuid' /= uuid)
              (state . cells)))

--------------------------------------------------------------------------------
-- API calls

refresh cells = do
  documentId <- H.liftEffect getDocumentId
  result <-
    rpcRefreshDocument
      (RefreshDocument
         { documentId: DocumentId documentId
         , document: InputDocument {cells: cells}
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

toInputCell :: OutputCell -> InputCell
toInputCell (OutputCell {uuid, name, code}) = InputCell {uuid, name, code}
