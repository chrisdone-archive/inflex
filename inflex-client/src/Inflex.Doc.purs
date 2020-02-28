module Inflex.Doc (component) where

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable
import Data.UUID
import Effect.Aff (launchAff)
import Effect.Class
import Effect.Class.Console (log)
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

data Command = Initialize

data Dec = Dec { dec :: Dec.Dec, uuid :: UUID }

type State = {
  decs :: Array Dec
 }

component :: forall q i o m. MonadEffect m =>  H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const { decs: [] }
    , render
    , eval:
        H.mkEval
          H.defaultEval {initialize = pure Initialize, handleAction = eval}
    }

eval =
  case _ of
    Initialize -> do
      decs <-
        traverse
          (\dec -> do
             uuid <- H.liftEffect genUUIDV4
             pure (Dec {uuid, dec}))
          initialDecs
      H.modify_ (\s -> s {decs = decs})
      pure unit

render =
  \state ->
    HH.div
      []
      (map
         (\(Dec {uuid, dec}) ->
            HH.slot
              (SProxy :: SProxy "Dec")
              (uuidToString uuid)
              Dec.component
              dec
              (const Nothing))
         (state . decs))

initialDecs =
  [Dec.Dec {name: "rate", rhs: "55.5"}
  ,Dec.Dec {name: "hours", rhs: "160"}
  ,Dec.Dec {name: "worked", rhs: "150"}
  ,Dec.Dec {name: "total", rhs: "worked / hours * rate"}]
