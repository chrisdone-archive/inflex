-- | Recursive editing of parts of a result.

module Inflex.Components.Cell.Editor
  ( Editor(..)
  , EditorAndCode(..)
  , Output(..)
  , Field(..)
  , Row(..)
  , Query(..)
  , component) where

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith, trim)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as Core
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Input as Input
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Inflex.Components.Cell.TextInput as TextInput
import Inflex.Components.Code as Code
import Inflex.FieldName (validFieldName)
import Inflex.Schema (CellError(..), FillError(..))
import Inflex.Schema as Shared
import Prelude (class Eq, class Ord, class Show, Unit, bind, const, discard, map, mempty, pure, show, unit, (&&), (+), (<<<), (<>), (==), (-))
import Web.DOM.Element (Element)
import Web.Event.Event (preventDefault, stopPropagation)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
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
  , cells :: Map UUID Shared.OutputCell
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
  | VegaElementChanged String (ElemRef' Element)
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
  | VegaE Shared.OriginalSource String
  | ErrorE CellError
  | ArrayE Shared.OriginalSource (Array Editor)
  | VariantE Shared.OriginalSource String (Maybe Editor)
  | RecordE Shared.OriginalSource (Array Field)
  | TableE Shared.OriginalSource
           (Array String)
           (Array Row)

editorOriginalSource :: Editor -> Shared.OriginalSource
editorOriginalSource =
  case _ of
    MiscE o _ -> o
    TextE o _ -> o
    VegaE o _ -> o
    ErrorE _ -> Shared.NoOriginalSource
    ArrayE o _ -> o
    RecordE o _ -> o
    TableE o _ _ -> o
    VariantE o _ _ -> o

derive instance genericEditor :: Generic Editor _
instance showEditor :: Show Editor where show x = genericShow x

data Display
  = DisplayEditor
  | DisplayCode

data EditorAndCode = EditorAndCode
  { editor :: Editor
  , code :: String
  , path :: Shared.DataPath -> Shared.DataPath
  , cells :: Map UUID Shared.OutputCell
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
  , code :: H.Slot Code.Query Code.Output Unit
  )

data Row = Row { fields :: Array Field, original :: Shared.OriginalSource}
         | HoleRow
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

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\input@(EditorAndCode {editor, code, path, cells}) ->
           State
             { display: DisplayEditor
             , editor
             , code
             , path
             , cellError: Nothing
             , lastInput: Just input
             , cells
             })
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
     forall a action m t0 t1 x. Ord t1 => (MonadAff m)
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

eval :: forall i t45 t48. MonadAff t45 => Command -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval cmd = do
  -- log (show cmd)
  eval' cmd

eval' :: forall i t45 t48. MonadAff t45 => Command -> H.HalogenM State t48 (Slots i) Output t45 Unit
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
    VegaElementChanged vegaSpec (ElemRef' elemRef) ->
      case elemRef of
        Created (element) ->
          case fromElement element of
            Just htmlelement -> H.liftEffect (vegaInPlace htmlelement vegaSpec)
            Nothing -> pure unit
        Removed _ -> pure unit
    StartEditor -> do
      H.modify_ (\(State st) -> State (st {display = DisplayCode}))
    FinishEditing code -> do
      -- log ("[FinishEditing] Got code:  "<> code)
      H.modify_ (\(State s) -> State (s {lastInput = Nothing, display = DisplayCode, code = code}))
      H.raise
        (NewCode
           (if trim code == ""
              then "_"
              else code))
    SetEditorInput input@(EditorAndCode {editor, code, path, cells}) -> do
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
               , cells
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
foreign import vegaInPlace :: HTMLElement -> String -> Effect Unit

--------------------------------------------------------------------------------
-- Render main component

render :: forall a. MonadAff a => State -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
render (State {display, code, editor, path, cellError, cells}) =
  case display of
    DisplayCode -> wrapper (renderControl <> errorDisplay)
    DisplayEditor ->
      if trim code == ""
        then wrapper (renderControl)
        else wrapper (renderEditor path cells editor)
  where
    renderControl =
      [ HH.slot
          (SProxy :: SProxy "code")
          unit
          Code.component
          (Code.Input
             { code:
                 if code == "_"
                   then ""
                   else code
             , cells
             })
          (case _ of
             Code.TextOutput text -> Just (FinishEditing text))
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
     forall a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> Map UUID Shared.OutputCell
  -> Editor
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderEditor path cells editor =
  case editor of
    MiscE _originalSource t ->
      [HH.div [HP.class_ (HH.ClassName "misc")] [HH.text t]]
    TextE _originalSource t ->
      [renderTextEditor path t]
    VegaE _originalSource t ->
      [renderVegaEditor path t]
    VariantE _originalSource tag arg ->
      [renderVariantEditor path cells tag arg]
    ErrorE msg -> [renderError msg]
    ArrayE _originalSource editors -> [renderArrayEditor path cells editors]
    RecordE _originalSource fields -> [renderRecordEditor path cells fields]
    TableE _originalSource columns rows -> renderTableEditor path cells columns rows

--------------------------------------------------------------------------------
-- Variant display

renderVariantEditor ::
     forall a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> Map UUID Shared.OutputCell
  -> String
  -> Maybe Editor
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderVariantEditor path cells tag marg =
  HH.div
    [HP.class_ (HH.ClassName "variant")]
    ([HH.div [HP.class_ (HH.ClassName "variant-tag")] [HH.text ("#" <> tag)]] <>
     case marg of
       Nothing -> []
       Just arg ->
         [HH.slot
            (SProxy :: SProxy "editor")
            ("#" <> show tag <> "/argument")
            component
            (EditorAndCode
               { editor: arg
               , code: editorCode arg
               , path: path <<< Shared.DataVariantOf tag
               , cells
               })
            (\output ->
               case output of
                 UpdatePath update -> Just (TriggerUpdatePath update)
                 NewCode rhs ->
                   Just
                     (TriggerUpdatePath
                        (Shared.UpdatePath
                           { path:
                               path (Shared.DataVariantOf tag Shared.DataHere)
                           , update:
                               Shared.CodeUpdate
                                 (Shared.Code {text: rhs})
                           })))])

--------------------------------------------------------------------------------
-- Vega display

renderVegaEditor ::
     forall i a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> String
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command
renderVegaEditor path vegaSpec =
  Keyed.div
    [HP.class_ (HH.ClassName "vega-keyed")]
    [ Tuple vegaSpec
            (HH.div
               [ HP.class_ (HH.ClassName "vega")
               , manage (VegaElementChanged vegaSpec <<< ElemRef')
               ]
               [])
    ]

--------------------------------------------------------------------------------
-- Text editor

renderTextEditor ::
     forall i a. MonadAff a
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
              , validator: const true
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

mapWithIndexNarrow :: forall b a. Int -> Int -> (Int -> a -> b) -> Array a -> Array b
mapWithIndexNarrow dropping taking' f xs =
  map
    (\(Tuple i x) -> f i x)
    (Array.take
       taking'
       (Array.drop dropping (Array.zip (Array.range 0 (Array.length xs - 1)) xs)))

renderTableEditor ::
     forall a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> Map UUID Shared.OutputCell
  -> Array String
  -> Array Row
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderTableEditor path cells columns rows =
  [ HH.table
      [HP.class_ (HH.ClassName "table")]
      [ tableHeading path columns emptyTable
      , HH.tbody
          [HP.class_ (HH.ClassName "table-body")]
          (bodyGuide emptyTable emptyRows <> mapWithIndex {-Narrow 0 10-} (tableRow columns path cells) rows <>
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

originateFromField :: Array String -> String -> String -> String
originateFromField columns key code =
  "{" <>
  joinWith
    ", "
    (map
       (\column ->
          show column -- TOOD: Make not bad. Fix this. Use JSON encode.
           <> ": " <>
          if column == key
            then code
            else "_")
       columns) <>
  "}"

tableRow ::
     forall a. MonadAff a
  => Array String
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID Shared.OutputCell
  -> Int
  -> Row
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
tableRow columns path cells rowIndex HoleRow =
  HH.tr
    []
    ([rowNumber rowIndex path] <>
     map
       (\key ->
          let editor' = MiscE Shared.NoOriginalSource "_"
           in HH.td
                [HP.class_ (HH.ClassName "table-datum-value")]
                [ HH.slot
                    (SProxy :: SProxy "editor")
                    (show rowIndex <> "/" <> key)
                    component
                    (EditorAndCode
                       { editor: editor'
                       , code: editorCode editor'
                       , cells
                       , path:
                           path <<<
                           Shared.DataElemOf rowIndex <<<
                           Shared.DataFieldOf key
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
                                            Shared.DataHere)
                                   , update:
                                       Shared.CodeUpdate
                                         (Shared.Code
                                            { text:
                                                originateFromField
                                                  columns
                                                  key
                                                  rhs
                                            })
                                   })))
                ])
       columns <>
     [addColumnBlank])
  where
    addColumnBlank = HH.td [HP.class_ (HH.ClassName "add-column-blank")] []

tableRow columns path cells rowIndex (Row {fields}) =
  HH.tr
    []
    ([rowNumber rowIndex path] <>
     Array.mapMaybe
       (\key0 ->
          case Array.find (\(Field{key}) -> key==key0) fields of
            Just (Field {key, value: editor'}) ->
              Just (HH.td
                      [HP.class_ (HH.ClassName "table-datum-value")]
                      [ HH.slot
                          (SProxy :: SProxy "editor")
                          (show rowIndex <> "/" <> key)
                          component
                          (EditorAndCode
                             { editor: editor'
                             , code: editorCode editor'
                             , cells
                             , path:
                                 path <<<
                                 Shared.DataElemOf rowIndex <<< Shared.DataFieldOf key
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
                                                     key
                                                     Shared.DataHere))
                                         , update:
                                             Shared.CodeUpdate
                                               (Shared.Code {text: rhs})
                                         })))
                      ])
            Nothing -> Nothing)
       columns <>
     [addColumnBlank])
  where
    addColumnBlank = HH.td [HP.class_ (HH.ClassName "add-column-blank")] []

rowNumber :: forall t19. Int -> (Shared.DataPath -> Shared.DataPath) -> HH.HTML t19 Command
rowNumber rowIndex path =
  HH.td
    [HP.class_ (HH.ClassName "row-number")]
    [ HH.div
        [HP.class_ (HH.ClassName "row-number-div")]
        [ HH.div
            [HP.class_ (HH.ClassName "row-number-text")]
            [HH.text (show (rowIndex + 1))]
        , HH.button
            [ HP.class_ (HH.ClassName "remove-row-button")
            , HE.onClick
                (\e ->
                   pure
                     (PreventDefault
                        (Event' (toEvent e))
                        (TriggerUpdatePath
                           (Shared.UpdatePath
                              { path: path Shared.DataHere
                              , update:
                                  Shared.RemoveUpdate
                                    (Shared.Removal
                                       {index: rowIndex})
                              }))))
            ]
            [HH.text "×"]
        ]
    ]

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
   MonadAff t628
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
    MonadAff t628
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
          , validator: validFieldName
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
     forall a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> Map UUID Shared.OutputCell
  -> Array Editor
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderArrayEditor path cells editors =
  HH.table
    [HP.class_ (HH.ClassName "array")]
    [HH.tbody [HP.class_ (HH.ClassName "array-body")] (body <> addNewRow)]
  where
    body =
      case editors of
        [] ->
          [ HH.tr
              []
              [ HH.td
                  [HP.colSpan 3, HP.class_ (HH.ClassName "array-empty")]
                  [HH.text "↙ Hit the bottom-left button to add rows!"]
              ]
          ]
        _ -> rows
    rows =
      mapWithIndex
        (\i editor' ->
           let childPath = path <<< Shared.DataElemOf i
            in HH.tr
                 []
                 [ rowNumber i path
                 , HH.td
                     [HP.class_ (HH.ClassName "array-datum-value")]
                     [ HH.slot
                         (SProxy :: SProxy "editor")
                         (show i)
                         component
                         (EditorAndCode
                            { editor: editor'
                            , code: editorCode editor'
                            , path: childPath
                            , cells
                            })
                         (\output ->
                            case output of
                              UpdatePath update ->
                                Just (TriggerUpdatePath update)
                              NewCode rhs ->
                                Just
                                  (TriggerUpdatePath
                                     (Shared.UpdatePath
                                        { path:
                                            childPath Shared.DataHere
                                        , update:
                                            Shared.CodeUpdate
                                              (Shared.Code
                                                 {text: rhs})
                                        })))
                     ]
                 ])
        editors
    addNewRow =
      [ HH.tr
          []
          [ HH.td
              [HP.class_ (HH.ClassName "add-row")]
              [ HH.button
                  [ HP.class_ (HH.ClassName ("add-row-button "))
                  , HP.title "Add row"
                  , HE.onClick
                      (\e ->
                         pure
                           (PreventDefault
                              (Event' (toEvent e))
                              (TriggerUpdatePath
                                 (Shared.UpdatePath
                                    { path: path Shared.DataHere
                                    , update:
                                        Shared.AddToEndUpdate
                                    }))))
                  ]
                  [HH.text "+"]
              ]
          , HH.td [HP.class_ (HH.ClassName "bottom-blank")] []
          ]
      ]

--------------------------------------------------------------------------------
-- Records

renderRecordEditor ::
     forall a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> Map UUID Shared.OutputCell
  -> Array Field
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderRecordEditor path cells fields =
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
     map
       (\(Field {key, value: editor'}) ->
          let childPath = path <<< Shared.DataFieldOf key
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
                    key
                    (TextInput.component
                      (TextInput.Config
                         { placeholder: "Type field name here"
                         , unfilled: "(empty field name)"
                         , title: "Click to edit field name"
                         , validator: validFieldName
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
                    key
                    component
                    (EditorAndCode
                       { editor: editor'
                       , cells
                       , code: editorCode editor'
                       , path: path <<< Shared.DataFieldOf key
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
    VegaE original s -> originalOr original s
    TextE original s -> (show s) -- TODO: Encoding strings is not easy. Fix this.
    ArrayE original xs -> ("[" <> joinWith ", " (map editorCode xs) <> "]")
    VariantE original tag marg -> ("#" <> tag <> arg)
      where arg = case marg of
                    Nothing -> ""
                    Just arg' -> "(" <> editorCode arg' <> ")"
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
                 (case _ of
                    Row {original: o,fields} -> RecordE o fields
                    HoleRow -> MiscE Shared.NoOriginalSource "_")
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
