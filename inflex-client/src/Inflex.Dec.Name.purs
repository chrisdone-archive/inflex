-- | A declaration in a document.

module Inflex.Dec.Name
  ( component
  ) where

import Data.Foldable (for_)
import Data.Functor (void)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Nullable
import Data.String
import Data.Symbol (SProxy(..))
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
import Prelude (Unit, pure, (<<<))
import Unsafe.Coerce
import Web.DOM.Element (Element, fromEventTarget)
import Web.Event.Event (preventDefault, stopPropagation, currentTarget)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (focus, fromElement)
import Web.UIEvent.KeyboardEvent as K
import Web.UIEvent.MouseEvent (toEvent)

--------------------------------------------------------------------------------
-- Component types

type Input = String

type Output = String

data State = State
  { name :: String
  , display :: Display
  }

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
        (\name -> State {name, display: DisplayResult})
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
              case toMaybe mvalue of
                Nothing -> H.liftEffect (log "No value...")
                Just value -> do
                  H.raise value
                  eval (SetInput value)
            Nothing -> pure unit
    StartEditor ->
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
            stopPropagation e) {-log "Triggering"-}
      eval c
    StopPropagation e c -> do
      H.liftEffect
            -- log "Preventing default and propagation ..."
        (do preventDefault e
            stopPropagation e) {-log "Triggering"-}
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
    DisplayResult -> HH.div [HE.onClick (\e -> pure StartEditor)] [HH.text name]
    DisplayEditor ->
      HH.input
        [ HP.class_ (HH.ClassName "form-control")
        , HP.placeholder "Name"
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
                   Just (StopPropagation (K.toEvent e) (CodeUpdate (K.toEvent e)))
                 code -> Just (StopPropagation (K.toEvent e) (PrintCode code)))
        ]
