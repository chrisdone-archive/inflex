-- |

module Inflex.Components.Doc
  ( component
  ) where

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Control.Monad.State (class MonadState)
import Data.Argonaut.Core (stringify) as J
import Data.Argonaut.Parser (jsonParser) as J
import Data.Either (Either(..))
import Data.Generic.Rep

import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe)

import Data.Symbol (SProxy(..))

import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUIDV4, uuidToString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, error)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON, class GenericDecode, class GenericEncode)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell as Cell
import Inflex.Json (opts)
import Inflex.Shared
import Prelude (Unit, bind, const, discard, map, mempty, pure, ($), (<>), show, unit, Unit, bind, const, discard, map, mempty, pure, show, unit, ($), (<>), class Show)

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
    Initialize -> do
      _ <- rpcLoadDocument
      pure unit
    NewCell -> do
      uuid <- H.liftEffect genUUIDV4
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let cells' =
            M.insert
              uuid
              (Cell.Cell
                 { name: "" -- "x_" <> replaceAll (Pattern "-") (Replacement "_") (uuidToString uuid)
                 , rhs: ""
                 , result: Left "..."
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

refresh cells = do
  H.modify_ (\s -> s {cells = cells})
  pure unit

-- TODO: Fix the double encoding and double decoding here.
rpcLoadDocument :: forall m. MonadAff m => m (Either String OutputDocument)
rpcLoadDocument =
  H.liftAff
    (do documentId <- H.liftEffect getDocumentId
        let endpoint = "/rpc"
        log ("POST " <> endpoint)
        case J.jsonParser (genericEncodeJSON opts (DocumentId documentId)) of
          Left e -> do
            error ("Own JSON was invalid! " <> e)
            pure (Left e)
          Right json -> do
            result <-
              H.liftAff
                (AX.post
                   ResponseFormat.json
                   endpoint
                   (Just (RequestBody.json json)))
            case result of
              Left err -> do
                error
                  ("POST " <> endpoint <>
                   " response failed to decode:" <>
                   AX.printError err)
                pure (Left (AX.printError err))
              Right response -> do
                log $
                  "POST " <> endpoint <> " response:" <>
                  (J.stringify (response . body))
                case runExcept
                       (genericDecodeJSON opts (J.stringify (response . body))) of
                  Right (r :: OutputDocument) -> do
                    log ("OK, decoded:" <> show r)
                    pure (Right r)
                  Left e -> do
                    error ("Failed to decode:" <> show e)
                    pure (Left (show e)))

-- TODO: Fix the double encoding and double decoding here.
rpcCall
  :: forall m input output
  .  MonadAff m
  => GenericEncode input
  => GenericDecode output
  => Generic input input
  => Generic output output
  => Show output
  => String
  -> input
  -> m (Either String output)
rpcCall endpoint input =
  H.liftAff
    (do documentId <- H.liftEffect getDocumentId
        log ("POST " <> endpoint)
        case J.jsonParser (genericEncodeJSON opts input) of
          Left e -> do
            error ("Own JSON was invalid! " <> e)
            pure (Left e)
          Right json -> do
            result <-
              H.liftAff
                (AX.post
                   ResponseFormat.json
                   endpoint
                   (Just (RequestBody.json json)))
            case result of
              Left err -> do
                error
                  ("POST " <> endpoint <>
                   " response failed to decode:" <>
                   AX.printError err)
                pure (Left (AX.printError err))
              Right response -> do
                log $
                  "POST " <> endpoint <> " response:" <>
                  (J.stringify (response . body))
                case runExcept
                       (genericDecodeJSON opts (J.stringify (response . body))) of
                  Right r -> do
                    log ("OK, decoded:" <> show r)
                    pure (Right r)
                  Left e -> do
                    error ("Failed to decode:" <> show e)
                    pure (Left (show e)))
