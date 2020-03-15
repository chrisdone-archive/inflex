-- | Recursive editing of parts of a result.

module Inflex.Editor
  ( Editor(..)
  , EditorAndCode(..)
  , component
  ) where

import Data.String
import Data.Foldable (for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, bind, discard, map, pure, unit, (<<<), (<>), (==))
import Halogen.Query.Input as Input
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Halogen.HTML.Core as Core
import Web.HTML.HTMLElement (focus, fromElement)
import Web.DOM.Element (Element)
import Web.Event.Event (preventDefault, stopPropagation)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent as K
import Web.UIEvent.MouseEvent (toEvent)

--------------------------------------------------------------------------------
-- Component types

type Input = EditorAndCode

type Output = String

data State = State
  { display :: Display
  , editor :: Editor
  , code :: String
  }

data Command
  = SetEditor EditorAndCode
  | StartEditor
  | SetInput String
  | FinishEditing String
  | PreventDefault Event Command
  | Autoresize
  | NoOp
  | InputElementChanged (ElemRef Element)

--------------------------------------------------------------------------------
-- Internal types

data Editor
  = IntegerE String
  | ArrayE (Array Editor)
  | MiscE String

editorCode :: Editor -> String
editorCode =
  case _ of
    IntegerE s -> s
    MiscE s -> s
    ArrayE xs -> "[" <> joinWith ", " (map editorCode xs) <> "]"

data Display
  = DisplayEditor
  | DisplayCode

data EditorAndCode = EditorAndCode
  { editor :: Editor
  , code :: String
  }

type Slots i = (editor::H.Slot i String Int)

data Edit
  = OverIndex Int Edit
  | SetCode String

manage :: forall r i. (ElemRef Element -> i) -> HP.IProp r i
manage act = HP.IProp (Core.Ref (Just <<< Input.Action <<< act))

--------------------------------------------------------------------------------
-- Constants

editorRef :: H.RefLabel
editorRef = (H.RefLabel "editor")

--------------------------------------------------------------------------------
-- Component

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState: (\(EditorAndCode{editor, code}) -> State {display: DisplayEditor, editor, code })
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval, receive = pure <<< SetEditor }
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall i t45 t48. MonadEffect t45 => Command -> H.HalogenM State t48 (Slots i) String t45 Unit
eval =
  case _ of
    InputElementChanged elemRef ->
      case elemRef of
        Created element ->
          case fromElement element of
            Just htmlelement -> H.liftEffect (focus htmlelement)
            Nothing -> pure unit
        Removed _ -> pure unit
    StartEditor -> do
      H.modify_ (\(State st) -> State (st {display = DisplayCode}))
    FinishEditing code -> do
      H.liftEffect (log ("Finish editing with code:" <> code))
      State {display, editor} <- H.get
      _result <- H.raise code
      H.modify_ (\(State st') -> State (st' {display = DisplayEditor}))
    SetInput i ->
      H.modify_ (\(State st) -> State (st {display = DisplayCode, code = i}))
    SetEditor (EditorAndCode {editor, code}) ->
      H.put (State {editor, code, display: DisplayEditor})
    Autoresize -> do
      ref <- H.getHTMLElementRef editorRef
      H.liftEffect (for_ ref (\el -> pure unit))
    PreventDefault e c -> do
      H.liftEffect
        (do log "Preventing default and propagation ..."
            preventDefault e
            stopPropagation e
            log "Triggering")
      eval c
    NoOp -> pure unit

--------------------------------------------------------------------------------
-- Render

render :: forall i a. MonadEffect a => State -> HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command
render (State {display, code, editor}) =
  case display of
    DisplayCode ->
      HH.div
        []
        [ HH.input
            [ HP.value code
            , HP.class_ (HH.ClassName "editor")
            , manage InputElementChanged
            , HE.onKeyUp
                (\k ->
                   case K.code k of
                     "Enter" -> Just (FinishEditing code)
                     _code -> Just Autoresize)
            , HE.onValueChange (\i -> pure (SetInput i))
            , HE.onClick (\e -> pure (PreventDefault (toEvent e) NoOp))
            ]
        ]
    DisplayEditor ->
      HH.span
        [ HE.onClick (\e -> pure (PreventDefault (toEvent e) StartEditor))
        , HP.class_ (HH.ClassName "clickable")
        ]
        [ let renderEditor =
                case _ of
                  IntegerE i -> HH.text i
                  ArrayE es ->
                    HH.table
                      [HP.class_ (HH.ClassName "array-editor")]
                      (mapWithIndex
                         (\i subEditor ->
                            HH.tr
                              []
                              [ HH.td
                                  [HP.class_ (HH.ClassName "stretch-child")]
                                  [ HH.slot
                                      (SProxy :: SProxy "editor")
                                      i
                                      component
                                      (EditorAndCode
                                         { editor: subEditor
                                         , code:
                                             editorCode subEditor
                                         })
                                      (\rhs ->
                                         Just
                                           (FinishEditing
                                              (editorCode
                                                 (ArrayE
                                                    (editArray i (MiscE rhs) es)))))
                                  ]
                              ])
                         es)
                  MiscE t -> HH.text t
           in renderEditor editor
        ]

editArray :: forall i. Int -> i -> Array i -> Array i
editArray idx i =
  mapWithIndex
    (\idx' oldi ->
       if idx == idx'
         then i
         else oldi)
