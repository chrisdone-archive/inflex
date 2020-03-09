-- | Recursive editing of parts of a result.

module Inflex.Editor
  ( Editor(..)
  , EditorAndCode(..)
  , component
  ) where

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
import Prelude (Unit, bind, discard, pure, unit, (<<<), (<>))
import Web.Event.Event (preventDefault)
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

--------------------------------------------------------------------------------
-- Internal types

data Editor
  = IntegerE String
  | ArrayE (Array Editor)
  | MiscE String

data Display
  = DisplayEditor
  | DisplayCode

data EditorAndCode = EditorAndCode
  { editor :: Editor
  , code :: String
  }

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

--------------------------------------------------------------------------------
-- Render

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
                            _code -> Just Autoresize)
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
                            (mapWithIndex (\i editor' ->
                                    HH.tr
                                      []
                                      [HH.td
                                         []
                                         [if false
                                             then renderEditor editor'
                                             else
                                               HH.slot (SProxy :: SProxy "editor")
                                                       i
                                                       component
                                                       (EditorAndCode {editor: editor', code})
                                                       (\rhs -> Just (FinishEditing rhs))]])
                                 es)
                 MiscE t -> HH.text t
         in renderEditor editor]
