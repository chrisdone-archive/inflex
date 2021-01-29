-- |

module Inflex.Components.CodeMirror
  ( component
  , noSelection
  , Input
  , Config(..)
  , InternalConfig
  , Range
  , Pos
  , Query
  , Output(..)
  , CMEvent(..)
  ) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
-- import Effect.Class.Console (warn, log)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, liftEffect, mkComponent, mkEval, put, raise, subscribe) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (effectEventSource, emit) as H
import Prelude (Unit, bind, const, discard, mempty, pure, unit, void, ($), (/=))
import Web.HTML.HTMLElement (HTMLElement)

--------------------------------------------------------------------------------
-- Types

type Input = Config

data Config = Config InternalConfig

type InternalConfig =
  { readOnly  :: Boolean
  , theme     :: String
  , selection :: Range
  , mode      :: String
  , value     :: String
  , styleActiveLine :: Boolean
  , lineNumbers :: Boolean
  , lineWrapping :: Boolean
  , autofocus :: Boolean
  , autoCloseBrackets :: Boolean
  , highlightSelectionMatches :: Boolean
  }

type Range =
  { head :: Pos
  , anchor :: Pos
  }

type Pos =
  { line :: Int
  , ch :: Int
  }

data State = State
  { codeMirror :: Maybe CodeMirror
  , config :: Config
  }

data Query a
  = Initializer a
  | Receive Config a
  | CMEventIn CMEvent

data Output
  = EnteredText String
  | CMEventOut CMEvent

data CMEvent
  = Focused
  | Blurred
  | CursorActivity
  | Enter

foreign import data CodeMirror :: Type

--------------------------------------------------------------------------------
-- Constants

refLabel :: H.RefLabel
refLabel = H.RefLabel "codemirror"

noSelection :: Range
noSelection = { head: { line : 0, ch : 0}, anchor: { line : 0, ch : 0}}

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval
              H.defaultEval
                { handleAction = eval
                , receive = receiver
                , initialize = Just (Initializer unit)
                }
    }
  where
    initialState :: Input -> State
    initialState (config) = State {codeMirror: Nothing, config}
    receiver (c) = Just (Receive c unit)

render :: forall t1 t3 t4. t1 -> HH.HTML t4 t3
render = const (HH.div [HP.ref refLabel] [])

eval :: forall t33 t35 t81. MonadEffect t33 => MonadAff t33 => Query Unit -> H.HalogenM State (Query t81) t35 Output t33 Unit
eval (CMEventIn event) =
  case event of
    Enter -> do
      State {codeMirror: c} <- H.get
      case c of
        Nothing -> do
          pure unit
        Just cm -> do
          value <- H.liftEffect $ getValue cm
          H.raise (EnteredText value)
    _ -> pure unit
eval (Initializer a) = do
  State {config: Config config} <- H.get
  melement <- H.getHTMLElementRef refLabel
  case melement of
    Nothing -> pure a
    Just element -> do
      cm <- H.liftEffect (codeMirror element config)
      void
        (H.subscribe
           (H.effectEventSource
              (\emitter -> do
                 setOnEnter cm (H.emit emitter (CMEventIn Enter))
                 setOnFocused cm (H.emit emitter (CMEventIn Focused))
                 setOnBlurred cm (H.emit emitter (CMEventIn Blurred))
                 setOnCursorActivity cm (H.emit emitter (CMEventIn CursorActivity))
                 pure mempty)))
      H.put
        (State
           { codeMirror: Just cm
           , config: Config config
           })
      pure a
eval (Receive (Config config') a) = do
  State {codeMirror: mcm, config: Config config} <- H.get
  case mcm of
    Just cm -> do
      if config' . value /= config . value
        then H.liftEffect (setValue cm (config' . value))
        else pure unit
      if config' . selection /= config . selection
        then H.liftEffect (scrollToLine cm (config . selection . head . line))
        else pure unit
    Nothing -> pure unit
  H.put (State {codeMirror: mcm, config: Config config'})
  pure a

--------------------------------------------------------------------------------
-- Foreign

foreign import codeMirror
  :: HTMLElement
  -> InternalConfig
  -> Effect CodeMirror

foreign import on
  :: CodeMirror
  -> String
  -> Effect Unit
  -> Effect Unit

foreign import getValue
  :: CodeMirror
  -> Effect String

foreign import getSelection
  :: CodeMirror
  -> Effect Range

foreign import setValue
  :: CodeMirror
  -> String
  -> Effect Unit

foreign import setOnEnter
  :: CodeMirror
  -> Effect Unit
  -> Effect Unit

foreign import setOnBlurred
  :: CodeMirror
  -> Effect Unit
  -> Effect Unit

foreign import setOnFocused
  :: CodeMirror
  -> Effect Unit
  -> Effect Unit

foreign import setOnCursorActivity
  :: CodeMirror
  -> Effect Unit
  -> Effect Unit

foreign import scrollToLine
  :: CodeMirror
  -> Int
  -> Effect Unit
