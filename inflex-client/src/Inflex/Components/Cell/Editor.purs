-- | Recursive editing of parts of a result.

module Inflex.Components.Cell.Editor
  ( Editor(..)
  , EditorAndCode(..)
  , Output(..)
  , Field(..)
  , Row(..)
  , Query(..)
  , component
  ) where

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith, trim)
import Data.Symbol (SProxy(..))
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
import Inflex.Components.Cell.TextInput as TextInput
import Inflex.Schema (CellError(..), FillError(..))
import Inflex.Schema as Shared
import Prelude
import Web.DOM.Element (Element)
import Web.Event.Event (preventDefault, stopPropagation)
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
  , cellError :: Maybe CellError
  , lastInput :: Maybe EditorAndCode
  }

data Command
  = SetEditorInput EditorAndCode
  | StartEditor
  | FinishEditing String
  | PreventDefault Event'
                   Command
  -- | Autoresize Event
  | NoOp
  | SetInput String
  | InputElementChanged (ElemRef' Element)
  | TriggerUpdatePath Shared.UpdatePath

data Query a =
  NestedCellError Shared.NestedCellError

derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show x = genericShow x

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
instance editorAndCodeEq :: Eq EditorAndCode where
  eq (EditorAndCode x) (EditorAndCode y) =
    show (x.editor)==show (y.editor) && x.code==y.code && x.path Shared.DataHere == y.path Shared.DataHere

instance showEditorAndCode :: Show EditorAndCode where
  show (EditorAndCode e) = "EditorAndCode{code=" <> show (e.code) <> "}"
newtype Event' = Event' Event
instance showEvent :: Show Event' where show _ = "Event"

newtype ElemRef' a = ElemRef' (ElemRef a)
instance showElemRef :: Show (ElemRef' a) where show _ = "ElemRef"

type Slots i =
  ( editor :: H.Slot i Output String
  , fieldname :: H.Slot i String String
  , textEditor :: H.Slot i String Unit
  )

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

component :: forall m. MonadEffect m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\input@(EditorAndCode {editor, code, path}) ->
           State {display: DisplayEditor, editor, code, path, cellError: Nothing, lastInput: Just input})
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetEditorInput
            , handleQuery = query
            }
    }

--------------------------------------------------------------------------------
-- Query

query ::
     forall a action m t0 t1 x. Ord t1 => (MonadEffect m)
  => Query a
  -> H.HalogenM State action (editor :: H.Slot Query t0 t1 | x) Output m (Maybe a)
query =
  case _ of
    NestedCellError cellError@(Shared.NestedCellError { path: errorPath
                                                       , error
                                                       }) -> do
      State {path} <- H.get
      let path' = path Shared.DataHere
      if path' == errorPath
        then do
          log ("[Editor] Received error at my path!: " <> show error)
          H.modify_
            (\(State st) ->
               State (st {display = DisplayCode, cellError = Just error}))
        else do
          _ <-
            H.queryAll (SProxy :: SProxy "editor") (NestedCellError cellError)
          pure unit
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

eval :: forall i t45 t48. MonadEffect t45 => Command -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval cmd = do
  -- log (show cmd)
  eval' cmd

eval' :: forall i t45 t48. MonadEffect t45 => Command -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval' =
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
      H.modify_ (\(State s) -> State (s {lastInput = Nothing}))
      H.raise
        (NewCode
           (if trim code == ""
              then "_"
              else code))
    SetEditorInput input@(EditorAndCode {editor, code, path}) -> do
      State state <- H.get
      case state . display of
        DisplayCode | pure input == state.lastInput -> pure unit -- Ignore if we're editing and input is the same.
        _ ->
          H.put
            (State
               { path
               , editor
               , code
               , display: DisplayEditor
               , cellError: Nothing
               , lastInput: Just input
               })
      -- do undefined
      --    H.put (State {path, editor, code, display:DisplayEditor, cellError:Nothing})
    {-Autoresize ev -> do
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
            Nothing -> pure unit-}
    PreventDefault (Event' e) c -> do
      H.liftEffect
        (do preventDefault e
            stopPropagation e)
      eval' c
    NoOp -> pure unit

foreign import getValue :: Element -> Effect (Nullable String)
foreign import setStyle :: String -> Element -> Effect Unit
foreign import autosize :: HTMLElement -> Effect Unit

--------------------------------------------------------------------------------
-- Render main component

render :: forall a. MonadEffect a => State -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
render (State {display, code, editor, path, cellError}) =
  case display of
    DisplayCode -> wrapper (renderControl <> errorDisplay)
    DisplayEditor ->
      if trim code == ""
        then wrapper (renderControl)
        else wrapper (renderEditor path editor)
  where
    renderControl =
      [ HH.input
          [ HP.value
              (if code == "_"
                 then ""
                 else code)
          , HP.class_ (HH.ClassName "form-control")
          , HP.placeholder "Type code here"
          , manage (InputElementChanged <<< ElemRef')
          , HE.onKeyUp
              (\k ->
                 case K.code k of
                   "Enter" -> Just (FinishEditing code)
                   _ -> Nothing)
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
                [ HP.class_
                    (HH.ClassName "editor-boundary-wrap clickable-to-edit")
                , HP.title "Click to edit"
                , HE.onClick
                    (\e ->
                       pure (PreventDefault (Event' (toEvent e)) StartEditor))
                ]
                inner
            _ ->
              HH.div
                [HP.class_ (HH.ClassName "editor-boundary-wrap")]
                ([ HH.div
                     [ HP.class_ (HH.ClassName "ellipsis-button")
                     , HP.title "Edit this as code"
                     , HE.onClick
                         (\e ->
                            pure
                              (PreventDefault (Event' (toEvent e)) StartEditor))
                     ]
                     []
                 ] <>
                 inner)
    errorDisplay =
      case cellError of
        Nothing -> []
        Just error -> [renderError error]

--------------------------------------------------------------------------------
-- Render inner editor

renderEditor ::
     forall a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Editor
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
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
renderTextEditor path text =
  HH.div
    [HP.class_ (HH.ClassName "text")]
    [ HH.slot
        (SProxy :: SProxy "textEditor")
        unit
        (TextInput.component
           (TextInput.Config
              { placeholder: "Type text here"
              , unfilled: "(empty text)"
              , title: "Click to edit text"
              }))
        (TextInput.Input {text, notThese: mempty})
        (\text' ->
           pure
             (TriggerUpdatePath
                (Shared.UpdatePath
                   { path: path Shared.DataHere
                   , update:
                       Shared.CodeUpdate
                         (Shared.Code {text: show text'})
                   })))
    ]

--------------------------------------------------------------------------------
-- Tables

renderTableEditor ::
     forall a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Array String
  -> Array Row
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderTableEditor path columns rows =
  [ HH.table
      [HP.class_ (HH.ClassName "table")]
      [ tableHeading path columns emptyTable
      , HH.tbody
          [HP.class_ (HH.ClassName "table-body")]
          (bodyGuide emptyTable emptyRows <> mapWithIndex (tableRow path) rows <>
           addNewRow)
      ]
  ]
  where
    emptyTable = Array.null columns && Array.null rows
    emptyRows = Array.null rows
    addNewRow =
      [ HH.tr
          []
          [ HH.td
              [HP.class_ (HH.ClassName "add-row")]
              [ HH.button
                  [ HP.class_
                      (HH.ClassName
                         ("add-row-button " <>
                          if disabled
                            then "disabled"
                            else ""))
                  , HP.title "Add row"
                  , HE.onClick
                      (\e ->
                         if disabled
                           then Nothing
                           else pure
                                  (PreventDefault
                                     (Event' (toEvent e))
                                     (TriggerUpdatePath
                                        (Shared.UpdatePath
                                           { path:
                                               path Shared.DataHere
                                           , update:
                                               Shared.AddToEndUpdate
                                           }))))
                  ]
                  [HH.text "+"]
              ]
          , HH.td
              [ HP.class_ (HH.ClassName "bottom-blank")
              , HP.colSpan
                  (Array.length columns + 1 +
                   (if emptyTable
                      then 1
                      else 0))
              ]
              []
          ]
      ]
      where
        disabled = Array.null columns
    fieldset = Set.fromFoldable columns

tableRow ::
     forall a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Int
  -> Row
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
tableRow path rowIndex (Row {original, fields}) =
  HH.tr
    []
    ([rowNumber] <>
     mapWithIndex
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
                     UpdatePath update -> Just (TriggerUpdatePath update)
                     NewCode rhs ->
                       Just
                         (TriggerUpdatePath
                            (Shared.UpdatePath
                               { path:
                                   path
                                     (Shared.DataElemOf
                                        rowIndex
                                        (Shared.DataFieldOf
                                           fieldIndex
                                           Shared.DataHere))
                               , update:
                                   Shared.CodeUpdate
                                     (Shared.Code {text: rhs})
                               })))
            ])
       fields <>
     [addColumnBlank])
  where
    addColumnBlank = HH.td [HP.class_ (HH.ClassName "add-column-blank")] []
    rowNumber =
      HH.td [HP.class_ (HH.ClassName "row-number")] [HH.text (show rowIndex)]

bodyGuide :: forall t651 t652. Boolean -> Boolean -> Array (HH.HTML t652 t651)
bodyGuide emptyTable emptyRows =
  if emptyTable
    then [ HH.tr
             []
             [ HH.td
                 [HP.colSpan 3, HP.class_ (HH.ClassName "table-empty")]
                 [HH.text "Hit the top-right button to add columns! ↗"]
             ]
         ]
    else if emptyRows
           then [ HH.tr
                    []
                    [ HH.td
                        [HP.colSpan 3, HP.class_ (HH.ClassName "table-empty")]
                        [HH.text "↙ Hit the bottom-left button to add rows!"]
                    ]
                ]
           else []

tableHeading :: forall t627 t628 t629.
   MonadEffect t628
  => (Shared.DataPath -> Shared.DataPath)
  -> Array String
  -> Boolean
  -> HH.HTML (H.ComponentSlot HH.HTML ( fieldname :: H.Slot t627 String String | t629) t628 Command) Command
tableHeading path columns emptyTable =
  HH.thead
    [HP.class_ (HH.ClassName "table-header")]
    ([HH.th [HP.class_ (HH.ClassName "table-column"), HP.title ""] []] <>
     mapWithIndex
       (\i text ->
          HH.th
            [HP.class_ (HH.ClassName "table-column"), HP.title "Click to edit"]
            [ HH.div
                [HP.class_ (HH.ClassName "table-column-content")]
                [columnNameSlot (Set.fromFoldable columns) path i text, removeColumnButton path text]
            ])
       columns <>
     (if emptyTable
        then [HH.th [] []]
        else []) <>
     [newColumnButton path columns])

columnNameSlot :: forall t627 t628 t629.
    MonadEffect t628
 => Set String -> (Shared.DataPath -> Shared.DataPath)
 -> Int
 -> String
 -> HH.HTML (H.ComponentSlot HH.HTML ( fieldname :: H.Slot t627 String String | t629) t628 Command) Command
columnNameSlot columns path i text =
  HH.slot
    (SProxy :: SProxy "fieldname")
    (show i)
    (TextInput.component
       (TextInput.Config
          { placeholder: "Type column name here"
          , unfilled: "(empty column name)"
          , title: "Click to edit column name"
          }))
    (TextInput.Input { text, notThese: columns } )
    (\name' ->
       pure
         (TriggerUpdatePath
            (Shared.UpdatePath
               { path: path (Shared.DataElemOf 0 Shared.DataHere)
               , update:
                   Shared.RenameFieldUpdate
                     (Shared.RenameField
                        {from: text, to: name'})
               })))

removeColumnButton ::
     forall t38.
     (Shared.DataPath -> Shared.DataPath)
  -> String
  -> HH.HTML t38 Command
removeColumnButton path text =
  HH.button
    [ HP.class_ (HH.ClassName "remove-column-button")
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
                          Shared.DeleteFieldUpdate
                            (Shared.DeleteField {name: text})
                      }))))
    ]
    [HH.text "×"]

newColumnButton ::
     forall t478.
     (Shared.DataPath -> Shared.DataPath)
  -> Array String
  -> HH.HTML t478 Command
newColumnButton path columns =
  HH.th
    [HP.class_ (HH.ClassName "add-column")]
    [ HH.button
        [ HP.class_ (HH.ClassName "add-column-button")
        , HP.title "Add column to this table"
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
                                (Shared.NewField
                                   {name: generateColumnName columns})
                          }))))
        ]
        [HH.text "+"]
    ]

-- | Search for a unique column name of the form "columnN".
generateColumnName :: Array String -> String
generateColumnName columns = iterate 1
  where
    set = Set.fromFoldable columns
    iterate i = if Set.member candidate set
                 then iterate (i+1)
                 else candidate
      where candidate = "column" <> show i

--------------------------------------------------------------------------------
-- Render arrays

renderArrayEditor ::
     forall a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Array Editor
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderArrayEditor path editors =
  HH.div
    [HP.class_ (HH.ClassName "array")]
    (case editors of
       [] -> [HH.text "(No items)"]
       _ ->
         mapWithIndex
           (\i editor' ->
              let childPath = path <<< Shared.DataElemOf i
              in
              HH.div
                [HP.class_ (HH.ClassName "array-item")]
                [ HH.slot
                    (SProxy :: SProxy "editor")
                    (show i)
                    component
                    (EditorAndCode
                       { editor: editor'
                       , code: editorCode editor'
                       , path: childPath
                       })
                    (\output ->
                       case output of
                         UpdatePath update -> Just (TriggerUpdatePath update)
                         NewCode rhs ->
                           Just
                             (TriggerUpdatePath
                                (Shared.UpdatePath
                                   { path: childPath Shared.DataHere
                                   , update:
                                       Shared.CodeUpdate
                                         (Shared.Code {text: rhs})
                                   })))
                ])
           editors)

--------------------------------------------------------------------------------
-- Records

renderRecordEditor ::
     forall a. MonadEffect a
  => (Shared.DataPath -> Shared.DataPath)
  -> Array Field
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
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
     (case fields of
        [] -> [HH.text "(No fields yet)"]
        _ -> []) <>
     mapWithIndex
       (\i (Field {key, value: editor'}) ->
          let childPath = path <<< Shared.DataFieldOf i
          in HH.tr
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
                    (TextInput.component
                      (TextInput.Config
                         { placeholder: "Type field name here"
                         , unfilled: "(empty field name)"
                         , title: "Click to edit field name"
                         }))
                    (TextInput.Input {text: key, notThese: Set.fromFoldable (map (\(Field{key: k}) -> k) fields)})
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
                             (TriggerUpdatePath
                                (Shared.UpdatePath
                                   { path: childPath Shared.DataHere
                                   , update:
                                       Shared.CodeUpdate
                                         (Shared.Code {text: rhs})
                                   })))
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
--
-- TODO: delete these functions and move all updates to server-side
-- path-based updates.

editorCode :: Editor -> String
editorCode =
  case _ of
    MiscE original s -> originalOr original s
    TextE original s -> (show s) -- TODO: Encoding strings is not easy. Fix this.
    ArrayE original xs -> ("[" <> joinWith ", " (map editorCode xs) <> "]")
    RecordE original fs ->
      ("{" <>
       joinWith
         ", "
         (map (\(Field {key, value}) -> key <> ":" <> editorCode value) fs) <>
       "}")
    ErrorE _ -> ""
    TableE original columns rows ->
      addTableTypeSig
        columns
        rows
        (editorCode
           (ArrayE
              original
              (map
                 (\(Row {original: o, fields}) ->
                    RecordE o fields)
                 rows)))

-- | Add a type signature if the rows are empty.
-- DONE: Consider whether this is the right place for this. It might cause trouble.
-- UPDATE: considered, seems fine.
addTableTypeSig :: forall a. Array String -> Array a -> String -> String
addTableTypeSig columns rows inner =
  case rows of
    [] -> "([] :: [{" <> joinWith "," columns <> "}])"
    _ -> inner

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
