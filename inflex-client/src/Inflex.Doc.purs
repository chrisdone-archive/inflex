module Inflex.Doc (component) where

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable
import Data.Tuple
import Data.UUID
import Effect
import Effect.Aff (launchAff)
import Effect.Aff.Class
import Effect.Class
import Effect.Class.Console (log)
import Foreign.Object as Foreign
import Halogen as H
import Halogen.HTML as HH
import Inflex.Dec as Dec
import Prelude

-- TODO:
--
-- 1. Make a Command type.
-- 2. Trigger in the @const Nothing@ below a parent command, indicating that this Dec was updated.
-- 3. Eval handler will simply send the whole lot via ajax to REST endpoint.
--    <https://github.com/purescript-halogen/purescript-halogen/blob/master/examples/effects-aff-ajax/src/Component.purs#L76>
-- 4. REST endpoint evaluates all decls, sends back new set of decls.
-- 5. Component updates internal state, causing re-render of views.
-- 6. Names of decs don't change, or they do, in which case unreferenced components are GC'd.

-- MORE EFFICIENT APPROACH TO RECONCILLIATION:
--
-- 1. Generate a UUID <https://pursuit.purescript.org/packages/purescript-uuid/6.0.1/docs/Data.UUID> for each decl-which-is-a-component in-a-document.
-- 2. Updates to each dec are routed to the same UUID, same component.
-- 3. A field in the input argument to dec component can be a SHA512
--    of the whole dec, causing a very fast equality comparison via
--    manual implementation of input handler in the Dec component.

-- NOTABLE: It's more expensive to delete/recreate a decl, than to
-- re-render and have the vdom differ update the small part that
-- changed.

-- IMPORTANT QUESTION: How does Halogen's components know when NOT to
-- re-render? Is it on H.modify?
-- ANSWER: It's H.modify (or H.put) <https://discourse.purescript.org/t/performance-in-halogen-components/456>

--
-- So we have 3 levels of avoiding work:
--
-- 1. Don't recreate components.
-- 2. Don't update component state if the SHA256 hasn't changed.
-- 3. Don't recreate the DOM, just modify in place via the VDOM.
--
-- Further microptimizations (depends on the size of the doc):
--
-- 1. Have the server only send those UUIDs that actually changed: it
--    knows the input, and knows the output. Easy to diff. This would
--    be 4 levels of avoiding work.

data Command = Initialize | UpdateDec UUID Dec.Dec

type State = {
  decs :: Map UUID Dec.Dec
 }

component :: forall q i o m. MonadEffect m =>  MonadAff m =>  H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { decs: mempty }
    , render
    , eval:
        H.mkEval
          H.defaultEval {initialize = pure Initialize, handleAction = eval}
    }

eval :: forall m. MonadState State m => MonadEffect m => MonadAff m => Command -> m Unit
eval =
  case _ of
    Initialize -> do
      decs <- H.liftEffect initialDecs
      case decOutsParser decs of
        Left e -> log e
        Right decs -> H.modify_ (\s -> s {decs = decs})
    UpdateDec uuid dec -> do
      H.liftEffect (log "Asking server for an update...")
      s <- H.get
      let decs' = M.insert uuid dec (s . decs)
      -- TODO Here is where we request from the server the latest results.
      result2 <-
        H.liftAff
          (AX.post
             ResponseFormat.json
             "/api/refresh"
             (Just
                (RequestBody.json
                   (J.fromObject
                      (Foreign.fromFoldable
                         (map
                            (\(Tuple uuid (Dec.Dec dec)) ->
                               Tuple
                                 (uuidToString uuid)
                                 (J.fromObject
                                    (Foreign.fromHomogeneous
                                       { name: J.fromString (dec . name)
                                       , rhs: J.fromString (dec . rhs)
                                       })))
                            (M.toUnfoldable decs') :: Array (Tuple String J.Json)))))))
      case result2 of
        Left err ->
          log $ "POST /api response failed to decode_=" <> AX.printError err
        Right response -> do
          log $ "POST /api response_=" <> J.stringify (response . body)
          case decOutsParser (response.body) of
            Left err -> log ("error parsing JSON:" <> err)
            Right decs'' -> H.modify_ (\s' -> s' {decs = decs''})

decOutsParser :: forall a. J.Json -> Either String (Map UUID (Dec.Dec))
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
                     maybe (Left "need name") (J.caseJsonString (Left "not a string") Right) (Foreign.lookup "name" dec)
                   rhs <-
                     maybe (Left "need rhs") (J.caseJsonString (Left "not a string") Right) (Foreign.lookup "rhs" dec)
                   result <-
                     maybe (Left "need result") (J.caseJsonString (Left "not a string") Right) (Foreign.lookup "result" dec)
                   case result of
                     "error" -> do error <-
                                     maybe (Left "need error") (J.caseJsonString (Left "not a string") Right) (Foreign.lookup "error" dec)
                                   pure (Dec.Dec {name, rhs, result: Left error})
                     "success" -> do
                       editor <-
                         maybe (Left "need editor")
                               (J.caseJsonObject
                                  (Left "expected editor obj")
                                  (\obj -> do
                                     typ <- maybe (Left "need type") (J.caseJsonString (Left "type not a string") Right) (Foreign.lookup "type" obj)
                                     case typ of
                                       "integer" -> do
                                         i <- maybe (Left "need integer") (J.caseJsonString (Left "integer not a string") Right) (Foreign.lookup "integer" obj)
                                         pure (Dec.IntegerE i)
                                       _ -> do i <- maybe (Left "need misc") (J.caseJsonString (Left "misc not a string") Right) (Foreign.lookup "misc" obj)
                                               pure (Dec.MiscE i)))
                               (Foreign.lookup "editor" dec)
                       pure (Dec.Dec {name, rhs, result: Right editor})
                     _ -> Left "invalid result"))
             mp)

render state =
  HH.div
    []
    (map
       (\(Tuple uuid dec) ->
          HH.slot
            (SProxy :: SProxy "Dec")
            (uuidToString uuid)
            Dec.component
            dec
            (\dec -> pure (UpdateDec uuid dec)))
       (M.toUnfoldable (state . decs)))

foreign import initialDecs :: Effect J.Json
