-- |

module Inflex.Components.Doc
  ( component
  ) where

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Control.Monad.State (class MonadState)
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe (Maybe)
import Data.String (Replacement(..), Pattern(..), replaceAll)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID(..), uuidToString, genUUIDV4)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, error)
import Foreign.Generic (defaultOptions, genericEncodeJSON, genericDecodeJSON, SumEncoding, class Decode, class Encode, genericEncode, genericDecode)
import Foreign.Object as Foreign
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell as Cell
import Prelude (Unit, bind, const, discard, map, mempty, pure, ($), (<>), show, unit)
import Prelude (class Show, Unit, bind, pure, unit)

--------------------------------------------------------------------------------
-- Foreign

foreign import getDocumentId :: Effect Int

--------------------------------------------------------------------------------
-- Types

data Command
  = Initialize
  | UpdateCell UUID
              Cell.Cell
  | NewCell
  | DeleteCell UUID

type State = {
  cells :: Map UUID Cell.Cell
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
   { cells :: Map UUID Cell.Cell | state }
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
           (\(Tuple uuid cell) ->
              HH.slot
                (SProxy :: SProxy "Cell")
                (uuidToString uuid)
                Cell.component
                cell
                (\cell' -> pure (case cell' of
                                  Cell.CelllUpdate d -> UpdateCell uuid d
                                  Cell.DeleteCelll -> DeleteCell uuid)))
           (M.toUnfoldable (state . cells)))
    ]

--------------------------------------------------------------------------------
-- Eval

eval :: forall m. MonadState State m => MonadEffect m => MonadAff m => Command -> m Unit
eval =
  case _ of
    Initialize -> load
    NewCell -> do
      uuid <- H.liftEffect genUUIDV4
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let cells' =
            M.insert
              uuid
              (Cell.Cell
                 { name: "x_" <> replaceAll (Pattern "-") (Replacement "_") (uuidToString uuid)
                 , rhs: "_"
                 , result: Left "New celll"
                 , new: true
                 })
              (s . cells)
      refresh cells'
    UpdateCell uuid cell -> do
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let cells' = M.insert uuid cell (s . cells)
      refresh cells'
    DeleteCell uuid -> do
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let cells' = M.delete uuid (s . cells)
      documentId <- H.liftEffect getDocumentId
      refresh cells'

--------------------------------------------------------------------------------
-- API calls

refresh _cells = do
  pure unit

load = do
  documentId <- H.liftEffect getDocumentId
  let endpoint = "/app/api/load/" <> show documentId
  log ("POST " <> endpoint)
  pure unit
  -- result <-
  --   H.liftAff
  --     (AX.post ResponseFormat.json endpoint (Just (RequestBody.json ?json)))
  -- case result of
  --   Left err ->
  --     error
  --       ("POST " <> endpoint <> " response failed to decode:" <>
  --        AX.printError err)
  --   Right response -> do
  --     log $
  --       "POST " <> endpoint <> " response:" <>
  --       J.stringify (response . body)
  --     H.modify_ (\s -> s {cells = ?cells})
