-- | A declaration in a document.

module Inflex.Components.Cell.Name
  ( component, Output(..), manage
  ) where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (trim)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as Core
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Input as Input
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Prelude
import Web.DOM.Element (Element, fromEventTarget)
import Web.Event.Event (preventDefault, stopPropagation, currentTarget)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent as ME
import Web.HTML.HTMLElement (focus, fromElement)
import Web.UIEvent.KeyboardEvent as K

--------------------------------------------------------------------------------
-- Component types

type Input = String

data Output
  = NewName String
  | NewOffset Int Int

data State = State
  { name :: String
  , display :: Display
  , mode :: UiMode
  }

data UiMode
  = Normal
  | Holding ME.MouseEvent
  | Dragging ME.MouseEvent

data Command
  = CodeUpdate Event
  | StartEditor
  | SetInput String
  | InputElementChanged (ElemRef Element)
  | PreventDefault Event
                   Command
  | StopPropagation Event
                    Command
  | NoOp
  | PrintCode String
  | SetHolding ME.MouseEvent
  | MouseMoveTrigger ME.MouseEvent
  | MouseUpTrigger
  | StopDragging

manage :: forall r i. (ElemRef Element -> i) -> HP.IProp r i
manage act = HP.IProp (Core.Ref (Just <<< Input.Action <<< act))

--------------------------------------------------------------------------------
-- Internal types

data Display
  = DisplayResult
  | DisplayEditor

--------------------------------------------------------------------------------
-- Component

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState:
        (\name -> State {name, display: DisplayResult, mode: Normal})
    , render
    , eval:
        H.mkEval H.defaultEval {handleAction = eval, receive = pure <<< SetInput}
    }

--------------------------------------------------------------------------------
-- Eval

foreign import getValue :: Element -> Effect (Nullable String)

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    SetHolding mouseEvent -> do
      H.liftEffect (log "Holding")
      H.modify_ (\(State s) -> State (s {mode = Holding mouseEvent}))
    StopDragging -> do
      H.liftEffect (log "Stop drag")
      H.modify_ (\(State s) -> State (s {mode = Normal}))
    MouseUpTrigger -> do
      State s <- H.get
      case s . mode of
        Dragging mouseEvent0 -> do
          H.liftEffect (log "Stop dragging")
          H.modify_ (\(State s') -> State (s' {mode = Normal}))
        _ -> do
          H.liftEffect (log "Normal, starting editor")
          H.modify_ (\(State s') -> State (s' {mode = Normal}))
          eval StartEditor
    MouseMoveTrigger mouseEvent -> do
      State s <- H.get
      case s . mode of
        Holding mouseEvent0 -> do
          H.liftEffect (log "Starting drag!")
          H.modify_ (\(State s') -> State (s' {mode = Dragging mouseEvent0}))
        Dragging mouseEvent0 -> do
          let x = ME.screenX mouseEvent - ME.screenX mouseEvent0
              y = ME.screenY mouseEvent - ME.screenY mouseEvent0
          H.raise (NewOffset x y)
          H.modify_ (\(State s') -> State (s' {mode = Dragging mouseEvent}))
        _ -> pure unit
    PrintCode string -> H.liftEffect (log string)
    CodeUpdate event -> do
      H.liftEffect (log "Ok, go!")
      case currentTarget event of
        Nothing -> pure unit
        Just x ->
          case fromEventTarget x of
            Just htmlelement -> do
              H.liftEffect (log "OK, got element.")
              mvalue <- H.liftEffect (getValue htmlelement)
              case toMaybe mvalue >>= cleanName of
                Nothing -> H.liftEffect (log "No value...")
                Just value -> do
                  H.raise (NewName value)
                  eval (SetInput value)
            Nothing -> pure unit
    StartEditor -> do
      void (H.modify (\(State s) -> State (s {display = DisplayEditor})))
    SetInput i -> do
      H.modify_ (\(State st) -> State (st {name = i, display = DisplayResult}))
    InputElementChanged elemRef ->
      case elemRef of
        Created element ->
          case fromElement element of
            Just htmlelement -> H.liftEffect (focus htmlelement)
            Nothing -> pure unit
        Removed _ -> pure unit
    PreventDefault e c -> do
      H.liftEffect
            -- log "Preventing default and propagation ..."
        (do preventDefault e
            stopPropagation e {-log "Triggering"-}
         )
      eval c
    StopPropagation e c -> do
      H.liftEffect
            -- log "Preventing default and propagation ..."
        (do preventDefault e
            stopPropagation e {-log "Triggering"-}
         )
      eval c
    NoOp -> pure unit

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit | keys) m Command)
                  Command
render (State {display, name}) =
  case display of
    DisplayResult ->
      HH.div
        [ HP.class_ (HH.ClassName "cell-name")
        , HE.onMouseDown (\e -> Just (StopPropagation (ME.toEvent e) (SetHolding e)))
        , HE.onMouseUp (\e -> Just (StopPropagation (ME.toEvent e) MouseUpTrigger))
        , HE.onMouseMove (\e -> Just (StopPropagation (ME.toEvent e) (MouseMoveTrigger e)))
        , HE.onMouseLeave (\e -> Just (StopPropagation (ME.toEvent e) StopDragging))
        ]
        [ case cleanName name of
            Nothing -> HH.text "(unnamed)"
            Just x -> HH.text x
        ]
    DisplayEditor ->
      HH.input
        [ HP.class_ (HH.ClassName "form-control")
        , HP.placeholder "Type name here"
        , manage InputElementChanged
        , HP.value name
        , HE.onKeyDown
            (\e ->
               case K.code e of
                 "Enter" -> Just (StopPropagation (K.toEvent e) NoOp)
                 code -> Nothing)
        , HE.onKeyUp
            (\e ->
               case K.code e of
                 "Enter" ->
                   Just
                     (StopPropagation (K.toEvent e) (CodeUpdate (K.toEvent e)))
                 code -> Just (StopPropagation (K.toEvent e) (PrintCode code)))
        ]

cleanName :: String -> Maybe String
cleanName x =
  if trim x == ""
    then Nothing
    else Just (trim x)
