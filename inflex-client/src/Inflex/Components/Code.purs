-- |

module Inflex.Components.Code
  ( component
  , Input(..)
  , Output(..)
  , Query(..)
  , Command(..)
  ) where

import Data.Array as Array
import Data.Foldable
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console
import Halogen as H
import Halogen.HTML as HH
import Inflex.Components.CodeMirror as CM
import Inflex.Lexer
import Prelude

--------------------------------------------------------------------------------
-- Interface

data Input = Input
  { code :: String
  , namesInScope :: Map String String -- uuid to title
  }

data Output =
  TextOutput String

data Query a = SetNamesInScope (Map String String)

--------------------------------------------------------------------------------
-- Internal protocol

data Command
  = HandleInput Input
  | CMEvent CM.CMEvent

data State = State
  { code :: String
  , namesInScope :: Map String String
  }

type Slots (i :: Type -> Type) =
  ( codemirror :: H.Slot CM.Query CM.Output Unit
  )

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\(Input input) ->
           State
             { code: input . code
             , namesInScope: input . namesInScope
             })
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
query =
  case _ of
    SetNamesInScope namesInScope -> do
      H.modify_ (\(State s) -> State (s {namesInScope = namesInScope}))
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

eval ::
     forall i t45 t48. MonadAff t45
  => Command
  -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval =
  case _ of
    HandleInput _ -> pure unit
    CMEvent event -> do
      case event of
        CM.Entered -> do
          mvalue <-
            H.query
              (SProxy :: SProxy "codemirror")
              unit
              (H.request CM.GetTextValue)
          case mvalue of
            Just value -> do
              H.raise (TextOutput value)
            Nothing -> pure unit
        _ -> pure unit

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
    CM.component
    (CM.Config
       { internalConfig:
       { readOnly: false
       , theme: "default"
       , selection: CM.noSelection
       , mode: "haskell"
       , value: state.code
       , styleActiveLine: true
       , lineNumbers: false
       , lineWrapping: true
       , autofocus: true
       , autoCloseBrackets: true
       , highlightSelectionMatches: true
       , namesInScope: map(\(Tuple _ v) -> v) (M.toUnfoldable (state.namesInScope))
       }
       , initializers: initializers})
    (case _ of
       CM.CMEventOut event -> Just (CMEvent event))
  where initializers =
          Array.mapMaybe
            (\token ->
               if (token.tag == "uuid") then
                 case M.lookup (token.text) (state.namesInScope) of
                   Nothing -> Nothing
                   Just text ->
                     Just (CM.MarkText
                           { line: token . location . start . line - 1
                           , ch: token . location . start . column - 1
                           }
                           { line: token . location . end . line - 1
                           , ch: token . location . end . column - 1
                           }
                           {
                             replaceText: text
                           })
               else
                 Nothing)
            (lexer (state.code))
