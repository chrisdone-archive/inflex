-- | The document area.

module Inflex.Doc (component) where

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState)
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.String (Replacement(..), Pattern(..), replaceAll)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID(..), uuidToString, genUUIDV4)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Object as Foreign
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Dec as Dec
import Inflex.Editor as Editor
import Prelude (Unit, bind, const, discard, map, mempty, pure, ($), (<>), show)

--------------------------------------------------------------------------------
-- Component types

data Command
  = Initialize
  | UpdateDec UUID
              Dec.Dec
  | NewDec
  | DeleteDecl UUID

type State = {
  decs :: Map UUID Dec.Dec
 }

type Input = Unit

type Output = Unit

--------------------------------------------------------------------------------
-- Foreign imports

foreign import initialDecs :: Effect J.Json

foreign import getDocumentId :: Effect Int

--------------------------------------------------------------------------------
-- Component

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState: const {decs: mempty}
    , render
    , eval:
        H.mkEval
          H.defaultEval {initialize = pure Initialize, handleAction = eval}
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall m. MonadState State m => MonadEffect m => MonadAff m => Command -> m Unit
eval =
  case _ of
    Initialize -> do
      decs <- H.liftEffect initialDecs
      case decOutsParser decs of
        Left e -> log e
        Right decs' -> H.modify_ (\s -> s {decs = decs'})
    NewDec -> do
      uuid <- H.liftEffect genUUIDV4
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let decs' =
            M.insert
              uuid
              (Dec.Dec
                 { name: "x_" <> replaceAll (Pattern "-") (Replacement "_") (uuidToString uuid)
                 , rhs: "_"
                 , result: Left "New decl"
                 , new: true
                 })
              (s . decs)
      documentId <- H.liftEffect getDocumentId
      result2 <-
        H.liftAff
          (AX.post
             ResponseFormat.json
             ("/app/api/refresh/" <> show documentId)
             (Just
                (RequestBody.json
                   (J.fromObject
                      (Foreign.fromFoldable
                         (map
                            (\(Tuple uuid' (Dec.Dec dec')) ->
                               Tuple
                                 (uuidToString uuid')
                                 (J.fromObject
                                    (Foreign.fromHomogeneous
                                       { name:
                                           J.fromString (dec' . name)
                                       , rhs:
                                           J.fromString (dec' . rhs)
                                       })))
                            (M.toUnfoldable decs') :: Array (Tuple String J.Json)))))))
      case result2 of
        Left err ->
          log $
          "POST /api response failed to decode:" <>
          AX.printError err
        Right response -> do
          log $
            "POST /api response:" <>
            J.stringify (response . body)
          case decOutsParser (response . body) of
            Left err -> log ("error parsing JSON:" <> err)
            Right decs'' -> H.modify_ (\s' -> s' {decs = decs''})
    UpdateDec uuid dec -> do
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let decs' = M.insert uuid dec (s . decs)
      documentId <- H.liftEffect getDocumentId
      result2 <-
        H.liftAff
          (AX.post
             ResponseFormat.json
             ("/app/api/refresh/" <> show documentId)
             (Just
                (RequestBody.json
                   (J.fromObject
                      (Foreign.fromFoldable
                         (map
                            (\(Tuple uuid' (Dec.Dec dec')) ->
                               Tuple
                                 (uuidToString uuid')
                                 (J.fromObject
                                    (Foreign.fromHomogeneous
                                       { name:
                                           J.fromString (dec' . name)
                                       , rhs:
                                           J.fromString (dec' . rhs)
                                       })))
                            (M.toUnfoldable decs') :: Array (Tuple String J.Json)))))))
      case result2 of
        Left err ->
          log $
          "POST /api response failed to decode:" <>
          AX.printError err
        Right response -> do
          log $
            "POST /api response:" <>
            J.stringify (response . body)
          case decOutsParser (response . body) of
            Left err -> log ("error parsing JSON:" <> err)
            Right decs'' -> H.modify_ (\s' -> s' {decs = decs''})
    DeleteDecl uuid -> do
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let decs' = M.delete uuid (s . decs)
      documentId <- H.liftEffect getDocumentId
      result2 <-
        H.liftAff
          (AX.post
             ResponseFormat.json
             ("/app/api/refresh/" <> show documentId)
             (Just
                (RequestBody.json
                   (J.fromObject
                      (Foreign.fromFoldable
                         (map
                            (\(Tuple uuid' (Dec.Dec dec')) ->
                               Tuple
                                 (uuidToString uuid')
                                 (J.fromObject
                                    (Foreign.fromHomogeneous
                                       { name:
                                           J.fromString (dec' . name)
                                       , rhs:
                                           J.fromString (dec' . rhs)
                                       })))
                            (M.toUnfoldable decs') :: Array (Tuple String J.Json)))))))
      case result2 of
        Left err ->
          log $
          "POST /api response failed to decode:" <>
          AX.printError err
        Right response -> do
          log $
            "POST /api response:" <>
            J.stringify (response . body)
          case decOutsParser (response . body) of
            Left err -> log ("error parsing JSON:" <> err)
            Right decs'' -> H.modify_ (\s' -> s' {decs = decs''})

decOutsParser :: J.Json -> Either String (Map UUID (Dec.Dec))
decOutsParser =
  J.caseJsonObject
    (Left "expected object of uuids")
    (\obj ->
       let mp =
             M.fromFoldable
               (map
                  (\(Tuple k v) -> Tuple (UUID k) v)
                  (Foreign.toUnfoldable obj :: Array (Tuple String J.Json))) :: Map UUID J.Json
        in traverse
             (J.caseJsonObject
                (Left "expected DecOut")
                (\dec -> do
                   name <-
                     maybe
                       (Left "need name")
                       (J.caseJsonString (Left "not a string") Right)
                       (Foreign.lookup "name" dec)
                   rhs <-
                     maybe
                       (Left "need rhs")
                       (J.caseJsonString (Left "not a string") Right)
                       (Foreign.lookup "rhs" dec)
                   result <-
                     maybe
                       (Left "need result")
                       (J.caseJsonString (Left "not a string") Right)
                       (Foreign.lookup "result" dec)
                   case result of
                     "error" -> do
                       error <-
                         maybe
                           (Left "need error")
                           (J.caseJsonString (Left "not a string") Right)
                           (Foreign.lookup "error" dec)
                       pure
                         (Dec.Dec
                            { name
                            , rhs
                            , result: Left error
                            , new: false
                            })
                     "success" -> do
                       editor <-
                         maybe
                           (Left "need editor")
                           (let editorParser =
                                  J.caseJsonObject
                                    (Left "expected editor obj")
                                    (\obj' -> do
                                       typ <-
                                         maybe
                                           (Left "need type")
                                           (J.caseJsonString
                                              (Left "type not a string")
                                              Right)
                                           (Foreign.lookup "type" obj')
                                       case typ of
                                         "integer" -> do
                                           i <-
                                             maybe
                                               (Left "need integer")
                                               (J.caseJsonString
                                                  (Left "integer not a string")
                                                  Right)
                                               (Foreign.lookup "integer" obj')
                                           pure (Editor.IntegerE i)
                                         "array" -> do
                                           es <-
                                             maybe
                                               (Left "need array")
                                               (J.caseJsonArray
                                                  (Left "not an array")
                                                  Right)
                                               (Foreign.lookup "array" obj')
                                           map
                                             Editor.ArrayE
                                             (traverse editorParser es)
                                         "row" -> do
                                           es <-
                                             maybe
                                               (Left "need row")
                                               (J.caseJsonObject
                                                  (Left "not a row")
                                                  (\fobj ->
                                                     map
                                                       foreignObjToMap
                                                       (traverse
                                                          editorParser
                                                          fobj)))
                                               (Foreign.lookup "row" obj')
                                           pure (Editor.RowE es)
                                         "cons" -> do
                                           name' <-
                                             maybe
                                               (Left "need name")
                                               (J.caseJsonString
                                                  (Left "name not a string")
                                                  Right)
                                               (Foreign.lookup "name" obj')
                                           slot <- maybe
                                                     (Left "need slot")
                                                     editorParser
                                                     (Foreign.lookup "slot" obj')
                                           pure (Editor.ConsE name' slot)
                                         _ -> do
                                           i <-
                                             maybe
                                               (Left "need misc")
                                               (J.caseJsonString
                                                  (Left "misc not a string")
                                                  Right)
                                               (Foreign.lookup "misc" obj')
                                           pure (Editor.MiscE i))
                             in editorParser)
                           (Foreign.lookup "editor" dec)
                       pure
                         (Dec.Dec
                            { name
                            , rhs
                            , result: Right editor
                            , new: false
                            })
                     _ -> Left "invalid result"))
             mp)

foreignObjToMap :: forall a. Foreign.Object a -> Map String a
foreignObjToMap fobj = M.fromFoldable (Foreign.toAscUnfoldable fobj :: Array (Tuple String a))

--------------------------------------------------------------------------------
-- Render

render :: forall q state keys m. MonadEffect m =>
   { decs :: Map UUID Dec.Dec | state }
   -> HH.HTML (H.ComponentSlot HH.HTML ( "Dec" :: H.Slot q Dec.Output String | keys) m Command) Command
render state =
  HH.div
    [HP.class_ (HH.ClassName "container-fluid")]
    [ HH.div
        [HP.class_ (HH.ClassName "row")]
        [ HH.button
            [ HP.class_ (HH.ClassName "mt-3 ml-3 mr-3 btn-primary btn")
            , HE.onClick (\e -> pure NewDec)
            ]
            [HH.text "New declaration"]
        ]
    , HH.div
        [HP.class_ (HH.ClassName "row")]
        (map
           (\(Tuple uuid dec) ->
              HH.slot
                (SProxy :: SProxy "Dec")
                (uuidToString uuid)
                Dec.component
                dec
                (\dec' -> pure (case dec' of
                                  Dec.DeclUpdate d -> UpdateDec uuid d
                                  Dec.DeleteDecl -> DeleteDecl uuid)))
           (M.toUnfoldable (state . decs)))
    ]