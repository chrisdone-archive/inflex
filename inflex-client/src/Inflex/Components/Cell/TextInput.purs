-- | A declaration in a document.

module Inflex.Components.Cell.TextInput
  ( component
  , Config(..)
  , Input(..)
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (trim)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
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
import Web.HTML.HTMLElement (focus, fromElement)
import Web.UIEvent.KeyboardEvent as K

--------------------------------------------------------------------------------
-- Component types

data Config = Config {
    placeholder :: String -- "Type name here"
  , unfilled :: String -- "(unnamed)"
  , title :: String -- "Click to edit name"
  , validator :: String -> Boolean
  }

newtype Input = Input {
  text :: String,
  notThese :: Set String
  }
derive instance eqInput :: Eq Input
derive instance genericInput :: Generic Input _
instance showInput :: Show Input where show = genericShow

type Output = String

data State = State
  { name :: String
  , display :: Display
  , notThese :: Set String
  , error :: Maybe Error
  , lastInput :: Input
  }

data Error
  = DuplicateName
  | InvalidValidation

data Command
  = CodeUpdate Event
  | StartEditor
  | SetInput Input
  | SetInputInternal Input
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

component :: forall q m. MonadEffect m => Config -> H.Component HH.HTML q Input Output m
component config =
  H.mkComponent
    { initialState:
        (\(lastInput@(Input {text: name, notThese})) ->
           State
             { name
             , display: DisplayResult
             , notThese: Set.delete name notThese
             , error: Nothing
             , lastInput
             })
    , render: render config
    , eval:
        H.mkEval
          H.defaultEval {handleAction = eval config, receive = pure <<< SetInput}
    }

--------------------------------------------------------------------------------
-- Eval

foreign import getValue :: Element -> Effect (Nullable String)

eval :: forall q i m. MonadEffect m => Config -> Command -> H.HalogenM State q i Output m Unit
eval config@(Config {validator}) =
  case _ of
    PrintCode string -> H.liftEffect (log string)
    CodeUpdate event -> do
      case currentTarget event of
        Nothing -> pure unit
        Just x ->
          case fromEventTarget x of
            Just htmlelement -> do
              mvalue <- H.liftEffect (getValue htmlelement)
              case toMaybe mvalue >>= cleanName of
                Nothing -> pure unit
                Just text -> do
                  State {notThese} <- H.get
                  if Set.member text notThese
                    then do
                      H.modify_
                        (\(State s) -> State (s {error = Just DuplicateName}))
                    else if validator text
                           then do
                             H.raise text
                             eval
                               config
                               (SetInputInternal (Input {text, notThese}))
                           else H.modify_
                                  (\(State s) ->
                                     State (s {error = Just InvalidValidation}))
            Nothing -> pure unit
    StartEditor ->
      void (H.modify (\(State s) -> State (s {display = DisplayEditor})))
    SetInput input -> do
      do
         State state <- H.get
         case state . display of
           DisplayEditor
             | state . lastInput == input -> do log ("SetInput: [skipped] " <> show input)
                                                pure unit
           _ -> do log ("SetInput:" <> show input)
                   eval config (SetInputInternal input)
    SetInputInternal (Input {text, notThese}) -> do
      H.modify_
        (\(State st) ->
           State
             (st
                { name = text
                , notThese = notThese
                , display = DisplayResult
                , error = Nothing
                }))
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
      eval config c
    StopPropagation e c -> do
      H.liftEffect
            -- log "Preventing default and propagation ..."
        (do preventDefault e
            stopPropagation e {-log "Triggering"-}
         )
      eval config c
    NoOp -> pure unit

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          Config -> State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit | keys) m Command)
                  Command
render (Config config) (State {display, name, error}) =
  case display of
    DisplayResult ->
      HH.div
        [ HP.class_ (HH.ClassName "cell-name")
        , HP.title (config.title)
        , HE.onClick (\e -> pure StartEditor)
        ]
        [ case cleanName name of
            Nothing -> HH.text (config.unfilled)
            Just x -> HH.text x
        ]
    DisplayEditor ->
      HH.div
        []
        ([ HH.input
             [ HP.class_ (HH.ClassName "form-control")
             , HP.placeholder (config.placeholder)
             , manage InputElementChanged
             , HP.value name
             -- , HE.onKeyDown
             --     (\e ->
             --        case K.code e of
             --          "Enter" -> Just (StopPropagation (K.toEvent e) NoOp)
             --          code -> Nothing)
             , HE.onKeyUp
                 (\e ->
                    case K.code e of
                      "Enter" ->
                        Just
                          (StopPropagation
                             (K.toEvent e)
                             (CodeUpdate (K.toEvent e)))
                      code -> Nothing {-Just (StopPropagation (K.toEvent e) (PrintCode code))-}
                  )
             ]
         ] <>
         case error of
           Nothing -> []
           Just err ->
             [HH.div
                [HP.class_ (HH.ClassName "error-message")]
                [ HH.text
                    (case err of
                       DuplicateName -> "already in use!"
                       InvalidValidation -> "invalid characters")
                ]])

cleanName :: String -> Maybe String
cleanName x =
  if trim x == ""
    then Nothing
    else Just (trim x)
