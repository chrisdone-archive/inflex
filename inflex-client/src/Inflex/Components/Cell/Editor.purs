-- | Recursive editing of parts of a result.

module Inflex.Components.Cell.Editor
  ( Editor(..)
  , EditorAndCode(..)
  , Output(..)
  , component
  ) where

import Data.Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, trim)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as Core
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Input as Input
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Inflex.Schema (CellError(..), FillError(..))
import Inflex.Schema as Shared
import Prelude (Unit, bind, discard, map, pure, unit, (<<<), (<>), (==), show)
import Web.DOM.Element (Element)
import Web.Event.Event (preventDefault, stopPropagation)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (focus, fromElement)
import Web.UIEvent.KeyboardEvent as K
import Web.UIEvent.MouseEvent (toEvent)

--------------------------------------------------------------------------------
-- Component types

type Input = EditorAndCode

data Output
  = NewCode String
  | AddFieldTo Shared.DataPath String

data State = State
  { display :: Display
  , editor :: Editor
  , code :: String
  , path :: Shared.DataPath
  }

data Command
  = SetEditor EditorAndCode
  | StartEditor
  | FinishEditing String
  | PreventDefault Event
                   Command
  | Autoresize
  | NoOp
  | SetInput String
  | InputElementChanged (ElemRef Element)
  | AddField Shared.DataPath String

--------------------------------------------------------------------------------
-- Internal types

data Editor
  = MiscE Shared.OriginalSource String
  | TextE Shared.OriginalSource String
  | ErrorE CellError
  | ArrayE Shared.OriginalSource (Array Editor)
  | RecordE Shared.OriginalSource (Array { key :: String, value :: Editor })
  | TableE Shared.OriginalSource
           (Array String)
           (Array { original :: Shared.OriginalSource
                  , fields :: Array { key :: String, value :: Editor }
                  }
           )

data Display
  = DisplayEditor
  | DisplayCode

data EditorAndCode = EditorAndCode
  { editor :: Editor
  , code :: String
  , path :: Shared.DataPath
  }

type Slots i = (editor :: H.Slot i Output String)

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
    { initialState: (\(EditorAndCode{editor, code, path}) -> State {display: DisplayEditor, editor, code, path })
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval, receive = pure <<< SetEditor }
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall i t45 t48. MonadEffect t45 => Command -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval =
  case _ of
    AddField path name -> H.raise (AddFieldTo path name)
    SetInput i -> do
      H.modify_ (\(State st) -> State (st {display = DisplayCode, code = i}))
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
      State {display, editor} <- H.get
      _result <-
        H.raise
          (NewCode (if trim code == ""
             then "_"
             else code))
      H.modify_ (\(State st') -> State (st' {display = DisplayEditor}))
    SetEditor (EditorAndCode {editor, code, path}) ->
      H.put (State {path, editor, code, display: DisplayEditor})
    Autoresize -> do
      ref <- H.getHTMLElementRef editorRef
      H.liftEffect (for_ ref (\el -> pure unit))
    PreventDefault e c -> do
      H.liftEffect
        (do preventDefault e
            stopPropagation e)
      eval c
    NoOp -> pure unit

--------------------------------------------------------------------------------
-- Render

render :: forall i a. MonadEffect a => State -> HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command
render (State {display, code, editor, path}) =
  case display of
    DisplayCode -> wrapper renderControl
    DisplayEditor ->
      if trim code == ""
        then wrapper (renderControl)
        else wrapper (renderEditor path editor)
  where
    renderControl =
      [ HH.input
          [ HP.value (if code == "_" then "" else code)
          , HP.class_ (HH.ClassName "form-control")
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
    wrapper inner =
      case display of
        DisplayCode -> HH.div [] inner
        DisplayEditor ->
          case editor of
            MiscE _ _ ->
              HH.div
                [ HP.class_ (HH.ClassName "editor-boundary-wrap clickable-to-edit")
                , HE.onClick
                    (\e -> pure (PreventDefault (toEvent e) StartEditor))
                ]
                inner
            _ ->
              HH.div
                [HP.class_ (HH.ClassName "editor-boundary-wrap")]
                ([ HH.div
                     [ HP.class_ (HH.ClassName "ellipsis-button")
                     , HE.onClick
                         (\e -> pure (PreventDefault (toEvent e) StartEditor))
                     ]
                     []
                 ] <>
                 inner)

renderEditor ::
     forall i a. MonadEffect a
  => Shared.DataPath
  -> Editor
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command)
renderEditor path editor =
  case editor of
    MiscE _originalSource t ->
      [HH.div
          [HP.class_ (HH.ClassName "misc")]
          [HH.text t]]
    TextE _originalSource t ->
       [HH.div
          [HP.class_ (HH.ClassName "text")]
          [HH.text t]]
    ErrorE msg ->
      [ HH.div
          [HP.class_ (HH.ClassName "error-message")]
          [ HH.text
              (case msg of
                 FillErrors fillErrors ->
                   joinWith ", " (map fromFillError fillErrors)
                   where fromFillError =
                           case _ of
                             NoSuchGlobal name ->
                               "missing name “" <> name <> "”"
                             OtherCellProblem name ->
                               "other cell “" <> name <> "” has a problem"
                 CyclicCells names ->
                   "cells refer to eachother in a loop:" <> " " <>
                   joinWith ", " names
                 DuplicateCellName -> "this name is used twice"
                 CellRenameErrors -> "internal bug; please report!" -- TODO:make this automatic.
                 CellTypeError -> "types of values don't match up"
                 CellStepEror -> "error while evaluating formula"
                 SyntaxError -> "syntax error, did you mistype something?")
          ]
      ]
    ArrayE _originalSource editors ->
      [ HH.div
          [HP.class_ (HH.ClassName "array")]
          (mapWithIndex
             (\i editor' ->
                HH.div
                  [HP.class_ (HH.ClassName "array-item")]
                  [ HH.slot
                      (SProxy :: SProxy "editor")
                      (show i)
                      component
                      (EditorAndCode
                         { editor: editor'
                         , code: editorCode editor'
                         , path: Shared.DataElemOf i path
                         })
                      (\output ->
                         case output of
                           AddFieldTo path f -> Just (AddField path f)
                           NewCode rhs -> Just
                            (FinishEditing
                              (editorCode
                                 (ArrayE Shared.NoOriginalSource (editArray i (MiscE Shared.NoOriginalSource rhs) editors)))))
                  ])
             editors)
      ]
    RecordE _originalSource fields ->
      [ HH.table
          [HP.class_ (HH.ClassName "record")]
          ((if true then [] else
              [HH.button [
                       HE.onClick
                    (\e -> pure (PreventDefault (toEvent e) (AddField path "foo")))
                       ] [HH.text "Add field"]]) <>
           mapWithIndex
             (\i {key, value: editor'} ->
                HH.tr
                  [HP.class_ (HH.ClassName "record-field")]
                  [ HH.td [HP.class_ (HH.ClassName "record-field-name")] [HH.text key]
                  , HH.td [HP.class_ (HH.ClassName "record-field-value")] [HH.slot
                      (SProxy :: SProxy "editor")
                      (show i)
                      component
                      (EditorAndCode
                         { editor: editor'
                         , code: editorCode editor'
                         , path: Shared.DataFieldOf i path
                         })
                      (\output ->
                       case output of
                         AddFieldTo path f -> Just (AddField path f)
                         NewCode rhs ->
                            Just
                           (FinishEditing
                              (editorCode
                                 (RecordE Shared.NoOriginalSource (editArray i {key, value: MiscE Shared.NoOriginalSource rhs} fields)))))]
                  ])
             fields)
      ]
    TableE _originalSource columns rows ->
      [ HH.table
        [HP.class_ (HH.ClassName "table")]
        [HH.thead [HP.class_ (HH.ClassName "table-header")]
                  (map (\text -> HH.th [HP.class_ (HH.ClassName "table-column")] [HH.text text]) columns)
        ,HH.tbody
           [HP.class_ (HH.ClassName "table-body")]
           (mapWithIndex
             (\rowIndex {original, fields} ->
               HH.tr []

               (mapWithIndex
                  (\fieldIndex {key, value: editor'} ->
                        HH.td [HP.class_ (HH.ClassName "table-datum-value")] [HH.slot
                           (SProxy :: SProxy "editor")
                           (show rowIndex <> "/" <> show fieldIndex)
                           component
                           (EditorAndCode
                              { editor: editor'
                              , code: editorCode editor'
                              , path: Shared.DataFieldOf fieldIndex (Shared.DataElemOf rowIndex path)
                              })
                           (\output ->
                             case output of
                              AddFieldTo path f -> Just (AddField path f)
                              NewCode rhs ->
                               Just
                                (FinishEditing
                                   (editorCode
                                      (TableE
                                         Shared.NoOriginalSource
                                         columns
                                         (editArray
                                            rowIndex
                                            {original: Shared.NoOriginalSource
                                            ,fields: editArray fieldIndex {key, value: MiscE Shared.NoOriginalSource rhs}
                                                       fields}
                                            rows)
                                       ))))]
                       )
                  fields)

               )
             rows)
        ]
      ]

editorCode :: Editor -> String
editorCode =
  case _ of
    MiscE original s -> originalOr original s
    TextE original s -> originalOr original s
    ArrayE original xs -> originalOr original ("[" <> joinWith ", " (map editorCode xs) <> "]")
    RecordE original fs ->
      originalOr original ("{" <> joinWith ", " (map (\{key,value} -> key <> ":" <> editorCode value) fs) <> "}")
    ErrorE _ -> ""
    TableE original _columns rows ->
      editorCode (ArrayE original (map (\{ original: o, fields } -> RecordE o fields) rows))

originalOr :: Shared.OriginalSource -> String -> String
originalOr Shared.NoOriginalSource s = s
originalOr (Shared.OriginalSource s) _ = s

editArray :: forall i. Int -> i -> Array i -> Array i
editArray idx i =
  mapWithIndex
    (\idx' oldi ->
       if idx == idx'
         then i
         else oldi)
