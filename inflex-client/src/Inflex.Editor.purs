-- |

module Inflex.Editor
  ( Editor(..)
  , EditorAndCode(..)
  , component
  ) where

import Data.Either
import Data.Foldable
import Data.FunctorWithIndex
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple
import Effect.Class
import Effect.Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Web.Event.Event (preventDefault, stopPropagation, stopImmediatePropagation)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement as Web
import Web.UIEvent.KeyboardEvent as K
import Web.UIEvent.MouseEvent (toEvent)

data Editor
  = IntegerE String
  -- | RationalE Rational
  -- | TextE Text
  -- | RecordE (HashMap Text Editor)
  -- | TableE (Vector Text) (Vector (HashMap Text Editor))
  | ArrayE (Array Editor)
  | MiscE String

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

data Display
  = DisplayEditor
  | DisplayCode

data EditorAndCode = EditorAndCode { editor :: Editor, code :: String }

component :: forall q o m. MonadEffect m => H.Component HH.HTML q EditorAndCode String m
component =
  H.mkComponent
    { initialState: (\(EditorAndCode{editor, code}) -> State {display: DisplayEditor, editor, code })
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval, receive = pure <<< SetEditor }
    }

render :: forall i a. MonadEffect a => State -> HH.HTML (H.ComponentSlot HH.HTML (editor::H.Slot i String Int) a Command) Command
render (State { display, code, editor }) =
  case display of
    DisplayCode ->
      HH.div [] [HH.input
                   [ HP.value code
                   , HP.class_ (HH.ClassName "editor")
                   , HP.ref editorRef
                   , HE.onKeyUp
                       (\k ->
                          case K.code k of
                            "Enter" -> Just (FinishEditing code)
                            code -> Just Autoresize)
                   , HE.onValueChange (\i -> pure (SetInput i))
                   ]
                ]
    DisplayEditor ->
      HH.span
        [HE.onClick (\e -> pure (PreventDefault (toEvent e) StartEditor))]
        [let renderEditor =
               case _ of
                 IntegerE i -> HH.text i
                 ArrayE es ->
                   HH.table [HP.class_ (HH.ClassName "array-editor")]
                            (mapWithIndex (\i editor ->
                                    HH.tr
                                      []
                                      [HH.td
                                         []
                                         [if false
                                             then renderEditor editor
                                             else
                                               HH.slot (SProxy :: SProxy "editor")
                                                       i
                                                       component
                                                       (EditorAndCode {editor, code})
                                                       (\rhs -> Just (FinishEditing rhs))]])
                                 es)
                 MiscE t -> HH.text t
         in renderEditor editor]

eval :: forall t45 t47 t48. MonadEffect t45 => Command -> H.HalogenM State t48 t47 String t45 Unit
eval =
  case _ of
    StartEditor -> do
      H.modify_ (\(State st) -> State (st { display = DisplayCode}))
    FinishEditing code -> do
      H.liftEffect (log ("Finish editing with code=" <> code))
      State {display, editor} <- H.get
      -- let newDec =
      --       let Dec dec = st . dec
      --        in Dec (dec {rhs = rhs, result = Left "Waiting..."})
      -- H.modify_
      --   (\(State st') -> State (st' {display = DisplayResult, dec = newDec}))
      _result <- H.raise code
      H.modify_ (\(State st') -> State (st' {display = DisplayEditor}))
    SetInput i ->
      H.modify_ (\(State st) -> State (st {display = DisplayCode, code = i}))
    SetEditor (EditorAndCode{editor,code}) ->
      H.put (State {editor, code, display: DisplayEditor})
    Autoresize -> do
      ref <- H.getHTMLElementRef editorRef
      -- TODO: Set style="width: 100ch"  where 100=length of input.
      H.liftEffect (for_ ref (\el -> pure unit))
    PreventDefault e c -> do
      H.liftEffect (preventDefault e)
      eval c

editorRef :: H.RefLabel
editorRef = (H.RefLabel "editor")
