-- | CodeMirror component.

module Inflex.Components.CodeMirror
  ( component
  , noSelection
  , Input
  , Config(..)
  , InternalConfig
  , Key(..)
  , Range
  , Pos
  , Query(..)
  , Output(..)
  , CMEvent(..)
  , KeyResult
  , keyHandled
  , keyPass
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Object
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, liftEffect, mkComponent, mkEval, put, raise, subscribe) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (effectEventSource, emit) as H
import Prelude (class Show, Unit, bind, const, discard, mempty, pure, unit, void, (/=), (<<<), (<>))
import Web.HTML.HTMLElement (HTMLElement)

--------------------------------------------------------------------------------
-- Interface

type Input = Config

data Output
  = CMEventOut CMEvent

data Query a
  = GetTextValue (String -> a)
  | AddKeyMap String (Object (Effect KeyResult))
  | RemoveKeyMap String

--------------------------------------------------------------------------------
-- Internal protocol

type Slots a = ()

data State = State
  { codeMirror :: Maybe CodeMirror
  , config :: Config
  }

data Command
  = Initializer
  | SetConfig Config
  | CMEventIn CMEvent

--------------------------------------------------------------------------------
-- Types

data Config = Config InternalConfig

type InternalConfig =
  { readOnly                  :: Boolean
  , theme                     :: String
  , selection                 :: Range
  , mode                      :: String
  , value                     :: String
  , styleActiveLine           :: Boolean
  , lineNumbers               :: Boolean
  , lineWrapping              :: Boolean
  , autofocus                 :: Boolean
  , autoCloseBrackets         :: Boolean
  , highlightSelectionMatches :: Boolean
  , extraKeys                 :: Object (Effect KeyResult)
  , namesInScope              :: Array String
  }

type Range =
  { head :: Pos
  , anchor :: Pos
  }

type Pos =
  { line :: Int
  , ch :: Int
  }

data CMEvent
  = Focused
  | Blurred
  | CursorActivity
  | KeyHandled Key
  | InputRead

derive instance genericCMEvent :: Generic CMEvent _
instance showCMEvent :: Show CMEvent where show x = genericShow x

data Key
  = Backspace
  | Up
  | Down
  | Enter


derive instance genericKey :: Generic Key _
instance showKey :: Show Key where show x = genericShow x

parseKey :: String -> Maybe Key
parseKey =
  case _ of
    "Backspace" -> Just Backspace
    "Up" -> Just Up
    "Down" -> Just Down
    "Enter" -> Just Enter
    _ -> Nothing

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
                , receive = pure <<< SetConfig
                , initialize = Just Initializer
                , handleQuery = query
                }
    }
  where
    initialState :: Input -> State
    initialState (config) = State {codeMirror: Nothing, config}

--------------------------------------------------------------------------------
-- Query

query ::
     forall i m a. (MonadAff m)
  => Query a
  -> H.HalogenM State Command (Slots i) Output m (Maybe a)
query =
  case _ of
    AddKeyMap string keys -> do
      State {codeMirror: c} <- H.get
      case c of
        Nothing -> pure unit
        Just cm -> H.liftEffect (addKeyMap cm string keys)
      pure Nothing
    RemoveKeyMap string -> do
      State {codeMirror: c} <- H.get
      case c of
        Nothing -> pure unit
        Just cm -> H.liftEffect (removeKeyMap cm string)
      pure Nothing
    GetTextValue reply -> do
      State {codeMirror: c} <- H.get
      case c of
        Nothing -> do
          pure Nothing
        Just cm -> do
          value <- H.liftEffect (getValue cm)
          pure (Just (reply value))

--------------------------------------------------------------------------------
-- Eval

eval :: forall t11 t9. MonadEffect t9 => MonadAff t9 => Command -> H.HalogenM State Command t11 Output t9 Unit
eval (CMEventIn event) =
  H.raise (CMEventOut event)
eval Initializer = do
  State {config: Config config} <- H.get
  melement <- H.getHTMLElementRef refLabel
  case melement of
    Nothing -> pure unit
    Just element -> do
      cm <- H.liftEffect (codeMirror element config)
      void
        (H.subscribe
           (H.effectEventSource
              (\emitter -> do
                 setOnFocused cm (H.emit emitter (CMEventIn Focused))
                 setOnBlurred cm (H.emit emitter (CMEventIn Blurred))
                 setOnCursorActivity
                   cm
                   (H.emit emitter (CMEventIn CursorActivity))
                 setOnKeyHandled
                   cm
                   (\name -> do
                      case parseKey name of
                        Just key -> H.emit emitter (CMEventIn (KeyHandled key))
                        Nothing -> log ("KeyHandled: Unknown key " <> name))
                 setOnInputRead cm (H.emit emitter (CMEventIn InputRead))
                 pure mempty)))
      H.put
        (State
           { codeMirror: Just cm
           , config: Config config
           })
eval (SetConfig (Config config')) = do
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

--------------------------------------------------------------------------------
-- Render

render :: forall t1 t3 t4. t1 -> HH.HTML t4 t3
render = const (HH.div [HP.ref refLabel] [])

--------------------------------------------------------------------------------
-- Foreign

foreign import codeMirror
  :: HTMLElement
  -> InternalConfig
  -> Effect CodeMirror

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

foreign import setOnKeyHandled
  :: CodeMirror
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import setOnInputRead
  :: CodeMirror
  -> Effect Unit
  -> Effect Unit

foreign import scrollToLine
  :: CodeMirror
  -> Int
  -> Effect Unit

foreign import addKeyMap
  :: CodeMirror
  -> String
  -> Object (Effect KeyResult)
  -> Effect Unit

foreign import removeKeyMap
  :: CodeMirror
  -> String
  -> Effect Unit

foreign import data KeyResult :: Type

foreign import keyHandled :: KeyResult

foreign import keyPass :: KeyResult
