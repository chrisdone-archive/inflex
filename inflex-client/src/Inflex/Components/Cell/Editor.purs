-- | Recursive editing of parts of a result.

module Inflex.Components.Cell.Editor
  ( Editor(..)
  , EditorAndCode(..)
  , Output(..)
  , Field(..)
  , Row(..)
  , component
  ) where

import Data.Array (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (joinWith, length, trim)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as Core
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Input as Input
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Inflex.Components.Cell.Name as Name
import Inflex.Schema (CellError(..), FillError(..))
import Inflex.Schema as Shared
import Prelude (class Show, Unit, bind, discard, map, max, pure, show, unit, (+), (<<<), (<>), (==))
import Web.DOM.Element (Element, fromEventTarget)
import Web.Event.Event (preventDefault, stopPropagation, currentTarget)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
import Web.UIEvent.KeyboardEvent as K
import Web.UIEvent.MouseEvent (toEvent)

--------------------------------------------------------------------------------
-- Component types

type Input = EditorAndCode

data Output
  = NewCode String
  | UpdatePath Shared.UpdatePath

data State = State
  { display :: Display
  , editor :: Editor
  , code :: String
  , path :: Shared.DataPath -> Shared.DataPath
  }

data Command
  = SetEditor EditorAndCode
  | StartEditor
  | FinishEditing String
  | PreventDefault Event'
                   Command
  | Autoresize Event
  | NoOp
  | SetInput String
  | InputElementChanged (ElemRef' Element)
  | TriggerUpdatePath Shared.UpdatePath

-- derive instance genericCommand :: Generic Command _
-- instance showCommand :: Show Command where show x = genericShow x

--------------------------------------------------------------------------------
-- Internal types

data Editor
  = MiscE Shared.OriginalSource String
  | TextE Shared.OriginalSource String
  | ErrorE CellError
  | ArrayE Shared.OriginalSource (Array Editor)
  | RecordE Shared.OriginalSource (Array Field)
  | TableE Shared.OriginalSource
           (Array String)
           (Array Row)

derive instance genericEditor :: Generic Editor _
instance showEditor :: Show Editor where show x = genericShow x

data Display
  = DisplayEditor
  | DisplayCode

data EditorAndCode = EditorAndCode
  { editor :: Editor
  , code :: String
  , path :: Shared.DataPath -> Shared.DataPath
  }

instance showEditorAndCode :: Show EditorAndCode where show _ = "EditorAndCode{}"
newtype Event' = Event' Event
instance showEvent :: Show Event' where show _ = "Event"

newtype ElemRef' a = ElemRef' (ElemRef a)
instance showElemRef :: Show (ElemRef' a) where show _ = "ElemRef"

type Slots i = (editor :: H.Slot i Output String, fieldname :: H.Slot i String String)

newtype Row = Row { fields :: Array Field, original :: Shared.OriginalSource}
derive instance genericRow :: Generic Row _
instance showRow :: Show Row where show x = genericShow x

newtype Field = Field { key :: String , value :: Editor}
derive instance genericField :: Generic Field _
instance showField :: Show Field where show x = genericShow x

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
    { initialState: (\(EditorAndCode{editor, code, path}) ->
                       State {display: DisplayEditor, editor, code, path })
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval, receive = pure <<< SetEditor }
    }

--------------------------------------------------------------------------------
-- Eval

-- eval' :: forall i t45 t48. MonadEffect t45 => Command -> H.HalogenM State t48 (Slots i) Output t45 Unit
-- eval' cmd = do
--   log (show cmd)
--   eval cmd

eval :: forall i t45 t48. MonadEffect t45 => Command -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval =
  case _ of
    TriggerUpdatePath update -> H.raise (UpdatePath update)
    SetInput i -> do
      H.modify_ (\(State st) -> State (st {display = DisplayCode, code = i}))
    InputElementChanged (ElemRef' elemRef) ->
      case elemRef of
        Created (element) ->
          case fromElement element of
            Just htmlelement -> H.liftEffect (autosize htmlelement)
            Nothing -> pure unit
        Removed _ -> pure unit
    StartEditor -> do
      H.modify_ (\(State st) -> State (st {display = DisplayCode}))
    FinishEditing code -> do
      State {display, editor} <- H.get
      _result <-
        H.raise
          (NewCode
             (if trim code == ""
                then "_"
                else code))
      H.modify_ (\(State st') -> State (st' {display = DisplayEditor}))
    SetEditor (EditorAndCode {editor, code, path}) ->
      H.put (State {path, editor, code, display: DisplayEditor})
    Autoresize ev -> do
      case currentTarget ev of
        Nothing -> pure unit
        Just x ->
          case fromEventTarget x of
            Just htmlelement -> do
              mvalue <- H.liftEffect (getValue htmlelement)
              case toMaybe mvalue of
                Nothing -> pure unit
                Just v ->
                  H.liftEffect
                    (setStyle
                       ("width:" <> show (max 3 (length v + 1)) <>
                        "ch")
                       htmlelement)
            Nothing -> pure unit
    PreventDefault (Event' e) c -> do
      H.liftEffect
        (do preventDefault e
            stopPropagation e)
      eval c
    NoOp -> pure unit

foreign import getValue :: Element -> Effect (Nullable String)
foreign import setStyle :: String -> Element -> Effect Unit
foreign import autosize :: HTMLElement -> Effect Unit

--------------------------------------------------------------------------------
-- Render main component

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
          , manage (InputElementChanged <<< ElemRef')
          -- , HP.
          , HE.onKeyUp
              (\k ->
                 case K.code k of
                   "Enter" -> Just (FinishEditing code)
                   _ -> Nothing
                   -- _code -> Just (Autoresize (K.toEvent k))
              )
          , HE.onValueChange (\i -> pure (SetInput i))
          , HE.onClick (\e -> pure (PreventDefault (Event' (toEvent e)) NoOp))
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
                    (\e -> pure (PreventDefault (Event' (toEvent e)) StartEditor))
                ]
                inner
            _ ->
              HH.div
                [HP.class_ (HH.ClassName "editor-boundary-wrap")]
                ([ HH.div
                     [ HP.class_ (HH.ClassName "ellipsis-button")
                     , HE.onClick
                         (\e -> pure (PreventDefault (Event' (toEvent e)) StartEditor))
                     ]
                     []
                 ] <>
                 inner)

--------------------------------------------------------------------------------
-- Render inner editor

renderEditor ::
     forall i a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Editor
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command)
renderEditor path editor =
  case editor of
    MiscE _originalSource t ->
      [HH.div [HP.class_ (HH.ClassName "misc")] [HH.text t]]
    TextE _originalSource t ->
      [renderTextEditor path t]
    ErrorE msg -> [renderError msg]
    ArrayE _originalSource editors -> [renderArrayEditor path editors]
    RecordE _originalSource fields -> [renderRecordEditor path fields]
    TableE _originalSource columns rows -> renderTableEditor path columns rows

--------------------------------------------------------------------------------
-- Text editor

renderTextEditor ::
     forall i a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> String
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command
renderTextEditor _path t = HH.div [HP.class_ (HH.ClassName "text")] [HH.text t]

--------------------------------------------------------------------------------
-- Tables

renderTableEditor ::
     forall i a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Array String
  -> Array Row
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command)
renderTableEditor path columns rows =
  [ HH.button
      [ HP.class_ (HH.ClassName "wip-button")
      , HE.onClick
          (\e ->
             pure
               (PreventDefault
                  (Event' (toEvent e))
                  (TriggerUpdatePath
                     (Shared.UpdatePath
                        { path:
                            path (Shared.DataElemOf 0 Shared.DataHere)
                        , update:
                            Shared.NewFieldUpdate
                              (Shared.NewField {name: "foo"})
                        }))))
      ]
      [HH.text "Add column"]
  , HH.table
      [HP.class_ (HH.ClassName "table")]
      [ HH.thead
          [HP.class_ (HH.ClassName "table-header")]
          (mapWithIndex
             (\i text ->
                HH.th
                  [HP.class_ (HH.ClassName "table-column")]
                  [ HH.slot
                      (SProxy :: SProxy "fieldname")
                      (show i)
                      Name.component
                      text
                      (\name' ->
                         pure
                           (TriggerUpdatePath
                              (Shared.UpdatePath
                                 { path:
                                     path (Shared.DataElemOf 0 Shared.DataHere)
                                 , update:
                                     Shared.RenameFieldUpdate
                                       (Shared.RenameField
                                          { from: text
                                          , to: name'
                                          })
                                 })))
                  , HH.button
                      [ HP.class_ (HH.ClassName "wip-button")
                      , HE.onClick
                          (\e ->
                             pure
                               (PreventDefault
                                  (Event' (toEvent e))
                                  (TriggerUpdatePath
                                     (Shared.UpdatePath
                                        { path:
                                            path
                                              (Shared.DataElemOf
                                                 0
                                                 Shared.DataHere)
                                        , update:
                                            Shared.DeleteFieldUpdate
                                              (Shared.DeleteField
                                                 {name: text})
                                        }))))
                      ]
                      [HH.text "-"]
                  ])
             columns)
      , HH.tbody
          [HP.class_ (HH.ClassName "table-body")]
          (mapWithIndex
             (\rowIndex (Row {original, fields}) ->
                HH.tr
                  []
                  (mapWithIndex
                     (\fieldIndex (Field {key, value: editor'}) ->
                        HH.td
                          [HP.class_ (HH.ClassName "table-datum-value")]
                          [ HH.slot
                              (SProxy :: SProxy "editor")
                              (show rowIndex <> "/" <> show fieldIndex)
                              component
                              (EditorAndCode
                                 { editor: editor'
                                 , code: editorCode editor'
                                 , path:
                                     path <<<
                                     Shared.DataElemOf rowIndex <<<
                                     Shared.DataFieldOf fieldIndex
                                 })
                              (\output ->
                                 case output of
                                   UpdatePath update ->
                                     Just (TriggerUpdatePath update)
                                   NewCode rhs ->
                                     Just
                                       (FinishEditing
                                          (editorCode
                                             (TableE
                                                Shared.NoOriginalSource
                                                columns
                                                (editArray
                                                   rowIndex
                                                     (Row { original:
                                                              Shared.NoOriginalSource
                                                          , fields:
                                                              editArray
                                                                fieldIndex
                                                                (Field
                                                                   { key
                                                                   , value:
                                                                       MiscE
                                                                         Shared.NoOriginalSource
                                                                         rhs
                                                                   })
                                                                fields
                                                          })
                                                   rows)))))
                          ])
                     fields))
             rows)
      ]
  ]

--------------------------------------------------------------------------------
-- Render arrays

renderArrayEditor ::
     forall i a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Array Editor
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command
renderArrayEditor path editors =
  HH.div
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
                   , path: path <<< Shared.DataElemOf i
                   })
                (\output ->
                   case output of
                     UpdatePath update -> Just (TriggerUpdatePath update)
                     NewCode rhs ->
                       Just
                         (FinishEditing
                            (editorCode
                               (ArrayE
                                  Shared.NoOriginalSource
                                  (editArray
                                     i
                                     (MiscE Shared.NoOriginalSource rhs)
                                     editors)))))
            ])
       editors)

--------------------------------------------------------------------------------
-- Records

renderRecordEditor ::
     forall i a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Array Field
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command
renderRecordEditor path fields =
  HH.table
    [HP.class_ (HH.ClassName "record")]
    ((if false
        then []
        else [ HH.button
                 [ HP.class_ (HH.ClassName "wip-button")
                 , HE.onClick
                     (\e ->
                        pure
                          (PreventDefault
                             (Event' (toEvent e))
                             (TriggerUpdatePath
                                (Shared.UpdatePath
                                   { path: path Shared.DataHere
                                   , update:
                                       Shared.NewFieldUpdate
                                         (Shared.NewField
                                            {name: "foo"})
                                   }))))
                 ]
                 [HH.text "Add field"]
             ]) <>
     mapWithIndex
       (\i (Field {key, value: editor'}) ->
          HH.tr
            [HP.class_ (HH.ClassName "record-field")]
            [ HH.td
                [HP.class_ (HH.ClassName "record-field-name")]
                [ HH.button
                    [ HP.class_ (HH.ClassName "wip-button")
                    , HE.onClick
                        (\e ->
                           pure
                             (PreventDefault
                                (Event' (toEvent e))
                                (TriggerUpdatePath
                                   (Shared.UpdatePath
                                      { path:
                                          path Shared.DataHere
                                      , update:
                                          Shared.DeleteFieldUpdate
                                            (Shared.DeleteField
                                               {name: key})
                                      }))))
                    ]
                    [HH.text "-"]
                                               -- HH.text key
                , HH.slot
                    (SProxy :: SProxy "fieldname")
                    (show i)
                    Name.component
                    key
                    (\name' ->
                       pure
                         (TriggerUpdatePath
                            (Shared.UpdatePath
                               { path: path Shared.DataHere
                               , update:
                                   Shared.RenameFieldUpdate
                                     (Shared.RenameField
                                        { from: key
                                        , to: name'
                                        })
                               })))
                ]
            , HH.td
                [HP.class_ (HH.ClassName "record-field-value")]
                [ HH.slot
                    (SProxy :: SProxy "editor")
                    (show i)
                    component
                    (EditorAndCode
                       { editor: editor'
                       , code: editorCode editor'
                       , path: path <<< Shared.DataFieldOf i
                       })
                    (\output ->
                       case output of
                         UpdatePath update -> Just (TriggerUpdatePath update)
                         NewCode rhs ->
                           Just
                             (FinishEditing
                                (editorCode
                                   (RecordE
                                      Shared.NoOriginalSource
                                      (editArray
                                         i
                                         (Field
                                            { key
                                            , value:
                                                MiscE
                                                  Shared.NoOriginalSource
                                                  rhs
                                            })
                                         fields)))))
                ]
            ])
       fields)

--------------------------------------------------------------------------------
-- Errors

renderError :: forall t10 t11. CellError -> HH.HTML t11 t10
renderError msg =
  HH.div
    [HP.class_ (HH.ClassName "error-message")]
    [ HH.text
        (case msg of
           FillErrors fillErrors -> joinWith ", " (map fromFillError fillErrors)
             where fromFillError =
                     case _ of
                       NoSuchGlobal name -> "missing name “" <> name <> "”"
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

--------------------------------------------------------------------------------
-- Code regenerators

editorCode :: Editor -> String
editorCode =
  case _ of
    MiscE original s -> originalOr original s
    TextE original s -> originalOr original (show s) -- TODO:Encoding strings is not easy. Fix this.
    ArrayE original xs ->
      originalOr original ("[" <> joinWith ", " (map editorCode xs) <> "]")
    RecordE original fs ->
      originalOr
        original
        ("{" <>
         joinWith
           ", "
           (map (\(Field {key, value}) -> key <> ":" <> editorCode value) fs) <>
         "}")
    ErrorE _ -> ""
    TableE original _columns rows ->
      editorCode
        (ArrayE
           original
           (map
              (\(Row {original: o, fields}) -> RecordE o fields)
              rows))

originalOr :: Shared.OriginalSource -> String -> String
originalOr Shared.NoOriginalSource s = s
originalOr (Shared.OriginalSource s) _ = s

-- TODO: This is slow -- use a 'update at index' function, one must
-- exist and be way faster.
editArray :: forall i. Int -> i -> Array i -> Array i
editArray idx i =
  mapWithIndex
    (\idx' oldi ->
       if idx == idx'
         then i
         else oldi)
