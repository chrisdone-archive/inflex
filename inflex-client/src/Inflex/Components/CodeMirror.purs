-- | CodeMirror component.

module Inflex.Components.CodeMirror
  ( component
  , noSelection
  , Input
  , Config(..)
  , InternalConfig
  , Range
  , Pos
  , Query(..)
  , Output(..)
  , CMEvent(..)
  , KeyResult
  , keyHandled
  , keyPass
  , MarkOptions

  ) where

import Data.Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign.Object (Object)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, liftEffect, mkComponent, mkEval, put, raise, subscribe) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (effectEventSource, emit) as H
import Prelude (class Show, Unit, bind, const, discard, mempty, pure, unit, void, (/=), (<<<))
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
  | MarkText Pos Pos MarkOptions

type MarkOptions = {
   replaceText :: String
 }

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

data Config = Config
  { internalConfig :: InternalConfig
  , initializers :: Array (Query Unit)
  }

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
  | Entered

derive instance genericCMEvent :: Generic CMEvent _
instance showCMEvent :: Show CMEvent where show x = genericShow x

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
    MarkText start end opts -> do
      State {codeMirror: c} <- H.get
      case c of
        Nothing -> do
          pure unit
        Just cm -> do
          H.liftEffect (markText cm start end opts)
      pure Nothing
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

eval :: forall i t9. MonadEffect t9 => MonadAff t9 => Command -> H.HalogenM State Command (Slots i) Output t9 Unit
eval (CMEventIn event) =
  H.raise (CMEventOut event)
eval Initializer = do
  State {config: Config {internalConfig, initializers}} <- H.get
  melement <- H.getHTMLElementRef refLabel
  case melement of
    Nothing -> pure unit
    Just element -> do
      cm <- H.liftEffect (codeMirror element internalConfig)
      H.put
        (State
           { codeMirror: Just cm
           , config:
               Config {internalConfig, initializers: []}
           })
      traverse_ query initializers
      void
        (H.subscribe
           (H.effectEventSource
              (\emitter -> do
                 setOnFocused cm (H.emit emitter (CMEventIn Focused))
                 setOnBlurred cm (H.emit emitter (CMEventIn Blurred))
                 setOnEntered cm (H.emit emitter (CMEventIn Entered))
                 pure mempty)))

eval (SetConfig (Config config')) = do
  State {codeMirror: mcm, config: Config config} <- H.get
  case mcm of
    Just cm -> do
      if config' . internalConfig . value /= config . internalConfig . value
        then H.liftEffect (setValue cm (config' . internalConfig . value))
        else pure unit
      if config' . internalConfig . selection /= config . internalConfig . selection
        then H.liftEffect (scrollToLine cm (config . internalConfig . selection . head . line))
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

foreign import setOnEntered
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

foreign import completionActive
  :: CodeMirror
  -> Effect Boolean

foreign import data KeyResult :: Type

foreign import keyHandled :: KeyResult

foreign import keyPass :: KeyResult

foreign import markText
  :: CodeMirror
  -> Pos
  -> Pos
  -> MarkOptions
  -> Effect Unit
