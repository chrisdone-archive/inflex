-- |

module Inflex.Components.Code
  ( component
  , Input(..)
  , Output(..)
  , Query(..)
  , Command(..)
  ) where

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Inflex.Components.CodeMirror as CodeMirror
import Prelude

--------------------------------------------------------------------------------
-- Interface

data Input = Input
  { code :: String
  }

data Output =
  TextOutput String

data Query a = SomeQuery

--------------------------------------------------------------------------------
-- Internal protocol

data Command
  = HandleInput Input
  | CMEvent CodeMirror.CMEvent

data State = State
  { code :: String
  }

type Slots (i :: Type -> Type) =
  ( codemirror :: H.Slot CodeMirror.Query CodeMirror.Output Unit
  )

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\(Input input) -> State {code: input . code})
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< HandleInput
            , handleQuery = query
            }
    }

--------------------------------------------------------------------------------
-- Query

query ::
     forall i m a. (MonadAff m)
  => Query a
  -> H.HalogenM State Command (Slots i) Output m (Maybe a)
query _ = pure Nothing

--------------------------------------------------------------------------------
-- Eval

eval ::
     forall i t45 t48. MonadAff t45
  => Command
  -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval =
  case _ of
    HandleInput _ -> pure unit
    CMEvent event ->
      case event of
        CodeMirror.Focused -> log "CodeMirror.Focused"
        CodeMirror.Blurred -> log "CodeMirror.Blurred"
        CodeMirror.CursorActivity -> log "CodeMirror.CursorActivity"
        CodeMirror.Enter -> do
          log "CodeMirror.Enter"
          mvalue <-
            H.query (SProxy :: SProxy "codemirror") unit (H.request CodeMirror.GetTextValue)
          case mvalue of
            Just value -> do H.raise (TextOutput value)
            Nothing -> pure unit

--------------------------------------------------------------------------------
-- Render

render ::
     forall a. MonadAff a
  => State
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
render (State state) =
  HH.slot
    (SProxy :: SProxy "codemirror")
    unit
    CodeMirror.component
    (CodeMirror.Config
       { readOnly: false
       , theme: "default"
       , selection: CodeMirror.noSelection
       , mode: "haskell"
       , value: state.code
       , styleActiveLine: true
       , lineNumbers: false
       , lineWrapping: true
       , autofocus: true
       , autoCloseBrackets: true
       , highlightSelectionMatches: true
       })
    (case _ of
       CodeMirror.CMEventOut event -> Just (CMEvent event))
