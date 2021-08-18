-- | Recursive editing of parts of a result.

module Inflex.Components.Cell.Editor
  ( EditorAndCode(..)
  , Output(..)
  , Field(..)
  , Row(..)
  , Query(..)
  , component) where

import Inflex.Frisson (View, caseCellError, caseDataPath, caseFillError, caseMaybeRow, caseOriginalSource, caseTree2, caseVariantArgument, field2Key, field2Value, nestedCellErrorError, nestedCellErrorPath, rowFields)
import Inflex.Types (OutputCell)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
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
import Inflex.Schema (CellError)
import Inflex.Schema as Shared
import Prelude
import Web.DOM.Element (Element)
import Web.Event.Event (preventDefault, stopPropagation)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
import Web.UIEvent.MouseEvent (toEvent)
import Web.UIEvent.WheelEvent (toEvent) as Wheel

--------------------------------------------------------------------------------
-- Component types

type Input = EditorAndCode

data Output
  = NewCode String
  | UpdatePath Shared.UpdatePath

data State = State
  { display :: Display
  , editor :: Either (View Shared.CellError) (View Shared.Tree2)
  , code :: String
  , path :: Shared.DataPath -> Shared.DataPath
  , cellError :: Maybe (View CellError)
  , cells :: Map UUID (OutputCell)
  , tableOffset :: Int
  , rowCount :: Int
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
  | ScrollTable Event'

data Query a
 = NestedCellError (View Shared.NestedCellError)
 | ResetDisplay

derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show x = genericShow x

--------------------------------------------------------------------------------
-- Internal types

data Display
  = DisplayEditor
  | DisplayCode

data EditorAndCode = EditorAndCode
  { editor :: Either (View Shared.CellError) (View Shared.Tree2)
  , code :: String
  , path :: Shared.DataPath -> Shared.DataPath
  , cells :: Map UUID (OutputCell)
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
         | HoleRow (View Shared.Tree2)
derive instance genericRow :: Generic Row _
instance showRow :: Show Row where show x = genericShow x

newtype Field = Field { key :: String , value :: View Shared.Tree2}
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
             , cells
             , tableOffset: 0
             , rowCount: getRowCount editor
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

getRowCount :: Either (View Shared.CellError) (View Shared.Tree2) -> Int
getRowCount editor =
  case editor of
    Left _ -> 0
    Right e ->
      caseTree2
         ((caseTree2Default 0)
          {
           "TableTreeMaybe2" = \_v _src flds rows ->
             Array.length rows
         })
         e

--------------------------------------------------------------------------------
-- Query

query ::
     forall a action m t0 t1 x. Ord t1 => (MonadAff m)
  => Query a
  -> H.HalogenM State action (editor :: H.Slot Query t0 t1 | x) Output m (Maybe a)
query =
  case _ of
    ResetDisplay -> do
      H.modify_ (\(State st) ->  (State (st {display = DisplayEditor})))
      pure Nothing
    NestedCellError cellError -> do
      State {path} <- H.get
      let path' = path Shared.DataHere
      if path' == materializePath (nestedCellErrorPath cellError)
        then do
          H.modify_
            (\(State st) ->
               State
                 (st
                    { display = DisplayCode
                    , cellError = Just (nestedCellErrorError cellError)
                    }))
        else do
          _ <-
            H.queryAll (SProxy :: SProxy "editor") (NestedCellError cellError)
          pure unit
      pure Nothing

materializePath :: View Shared.DataPath -> Shared.DataPath
materializePath = caseDataPath {
   "DataHere": Shared.DataHere,
   "DataElemOf": \i d -> Shared.DataElemOf i (materializePath d),
   "DataFieldOf": \i d -> Shared.DataFieldOf i (materializePath d),
   "DataVariantOf": \i d -> Shared.DataVariantOf i (materializePath d)
  }

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
      H.modify_ (\(State s) -> State (s {code = code, display = DisplayCode}))
      H.raise
        (NewCode
           (if trim code == ""
              then "_"
              else code))
    SetEditorInput input@(EditorAndCode {editor, code, path, cells}) -> do
      State state <- H.get
      H.put
            (State
               { path
               , editor
               , code
               , display: DisplayEditor
               , cellError: Nothing
               , cells
               , tableOffset: 0
               , rowCount: getRowCount editor
               })
    ScrollTable (Event' e) -> do
      delta <- H.liftEffect (getDeltaY e)
      if delta > 0.0
         then H.modify_ (\(State s) -> State (s { tableOffset = min (s.rowCount - pageSize) (s.tableOffset + 1) }))
         else H.modify_ (\(State s) -> State (s { tableOffset = max 0 (s.tableOffset - 1) }))
    PreventDefault (Event' e) c -> do
      H.liftEffect
        (do preventDefault e
            stopPropagation e)
      eval' c
    NoOp -> pure unit

foreign import getDeltaY :: Event -> Effect Number
foreign import getValue :: Element -> Effect (Nullable String)
foreign import setStyle :: String -> Element -> Effect Unit
foreign import autosize :: HTMLElement -> Effect Unit
foreign import vegaInPlace :: HTMLElement -> String -> Effect Unit

--------------------------------------------------------------------------------
-- Render main component

render :: forall a. MonadAff a => State -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
render (State {display, code, editor, path, cellError, cells, tableOffset}) =
  case display of
    DisplayCode -> wrapper (renderControl <> errorDisplay)
    DisplayEditor ->
      if blank code editor
        then wrapper (renderControl)
        else wrapper (case editor of
                        Left msg -> [renderError msg]
                        Right e -> renderEditor tableOffset path cells e)
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
        DisplayCode -> HH.div [] inner -- Should not be happening.
        DisplayEditor ->
          let boundaryWrap =
                (HH.div
                     [HP.class_ (HH.ClassName "editor-boundary-wrap")]
                     ([ HH.div
                          [ HP.class_ (HH.ClassName "ellipsis-button")
                          , HP.title "Edit this as code"
                          , HE.onClick
                              (\ev ->
                                 pure
                                   (PreventDefault (Event' (toEvent ev)) StartEditor))
                          ]
                          []
                      ] <>
                      inner))
          in case editor of
             Left _ -> boundaryWrap
             Right e ->
               caseTree2
                  ((caseTree2Default
                   boundaryWrap)
                   {
                    "MiscTree2" = \v _originalSource t ->
                      HH.div
                        [ HP.class_
                            (HH.ClassName "editor-boundary-wrap clickable-to-edit")
                        , HP.title "Click to edit"
                        , HE.onClick
                            (\ev ->
                               pure (PreventDefault (Event' (toEvent ev)) StartEditor))
                        ]
                        inner
                  })
                  e
    errorDisplay =
      case cellError of
        Nothing -> []
        Just error -> [renderError error]

blank :: forall e. String -> Either e (View Shared.Tree2) -> Boolean
blank code editor =
  if code == ""
    then case editor of
           Left _ -> true
           _ -> false
    else false

--------------------------------------------------------------------------------
-- Render inner editor

renderEditor ::
     forall a. MonadAff a
  => Int -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> View Shared.Tree2
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderEditor offset path cells =
  caseTree2 {
    "MiscTree2":       \v _originalSource t ->
      [HH.div [HP.class_ (HH.ClassName "misc")] [HH.text t]],
    "TextTree2":       \v _originalSource t ->
      [renderTextEditor path t],
    "VegaTree2":       \v _originalSource t ->
      [renderVegaEditor path t],
    "VariantTree2":    \v _originalSource tag arg ->
      [renderVariantEditor path cells tag arg],
    "ArrayTree2":      \v _originalSource editors ->
      [renderArrayEditor path cells editors],
    "RecordTree2":     \v _originalSource fields ->
      [renderRecordEditor path cells fields],
    "TableTreeMaybe2": \v _originalSource columns rows ->
      renderTableEditor offset path cells columns rows,
    "HoleTree": [HH.div [HP.class_ (HH.ClassName "hole"), HP.title "A blank hole"] [HH.text "_"]]
  }

--------------------------------------------------------------------------------
-- Variant display

renderVariantEditor ::
     forall a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> String
  -> View Shared.VariantArgument
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderVariantEditor path cells tag marg =
  HH.div
    [HP.class_ (HH.ClassName "variant")]
    ([HH.div [HP.class_ (HH.ClassName "variant-tag")] [HH.text ("#" <> tag)]] <>
    caseVariantArgument
      {"NoVariantArgument": [],
       "VariantArgument": \arg ->
         [HH.slot
            (SProxy :: SProxy "editor")
            ("#" <> show tag <> "/argument")
            component
            (EditorAndCode
               { editor: Right arg
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
                           })))
               ]
      }
      marg)

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

pageSize :: Int
pageSize = 10

renderTableEditor ::
     forall a. MonadAff a
  => Int
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Array String
  -> Array (View Shared.MaybeRow)
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderTableEditor offset path cells columns rows =
  [ HH.table
      [HP.class_ (HH.ClassName "table")]
      [ tableHeading path columns emptyTable
      , HH.tbody
          [HP.class_ (HH.ClassName "table-body"), HE.onWheel (\e -> Just (PreventDefault
                                                                   (Event' (Wheel.toEvent e))
                                                                   (ScrollTable (Event' (Wheel.toEvent e)))))]
          (bodyGuide emptyTable emptyRows <>
           mapWithIndexNarrow offset pageSize (\i -> caseMaybeRow {
             "SomeRow": tableRow columns path cells i
            ,"HoleRow": tableRowHole columns path cells i
           }) rows <>
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

tableRowHole ::
     forall a. MonadAff a
  => Array String
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Int
  -> View Shared.Tree2
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
tableRowHole columns path cells rowIndex editor' =
  HH.tr
    []
    ([rowNumber rowIndex path] <>
     map
       (\key ->
          HH.td
                [HP.class_ (HH.ClassName "table-datum-value")]
                [ HH.slot
                    (SProxy :: SProxy "editor")
                    (show rowIndex <> "/" <> key)
                    component
                    (EditorAndCode
                       { editor: Right editor'
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

tableRow ::
     forall a. MonadAff a
  => Array String
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Int
  -> View Shared.Row
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
tableRow columns path cells rowIndex row =
  HH.tr
    []
    ([rowNumber rowIndex path] <>
     Array.mapWithIndex
       (\idx editor' ->
          case Array.index columns idx of
            Just key ->
              HH.td
                [HP.class_ (HH.ClassName "table-datum-value")]
                [ HH.slot
                    (SProxy :: SProxy "editor")
                    (show rowIndex <> "/" <> key)
                    component
                    (EditorAndCode
                       { editor: Right editor'
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
                ]
            Nothing -> HH.td [] [HH.text "BUG!"])
       (rowFields row) <>
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
                                   {name: generateColumnName "column" columns})
                          }))))
        ]
        [HH.text "+"]
    ]

-- | Search for a unique column name of the form "columnN".
generateColumnName :: String -> Array String -> String
generateColumnName prefix columns = iterate 1
  where
    set = Set.fromFoldable columns
    iterate i = if Set.member candidate set
                 then iterate (i+1)
                 else candidate
      where candidate = prefix <> show i

--------------------------------------------------------------------------------
-- Render arrays

renderArrayEditor ::
     forall a. MonadAff a
  => (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Array (View Shared.Tree2)
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
                            { editor: Right editor'
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
  -> Map UUID (OutputCell)
  -> Array (View Shared.Field2)
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderRecordEditor path cells fields =
  HH.table
    [HP.class_ (HH.ClassName "record")]
    (([ HH.button
          [ HP.class_ (HH.ClassName "add-field-button")
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
                                     { name:
                                         generateColumnName
                                           "field"
                                           (map field2Key fields)
                                     })
                            }))))
          , HP.title "Add field"
          ]
          [HH.text "+"]
      ]) <>
     map
       (\field ->
          let key = field2Key field
              editor' = field2Value field
           in let childPath = path <<< Shared.DataFieldOf key
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
                                                       { name:
                                                           key
                                                       })
                                              }))))
                            ]
                            [HH.text "-"]
                                               -- HH.text key
                        , HH.slot
                            (SProxy :: SProxy "fieldname")
                            key
                            (TextInput.component
                               (TextInput.Config
                                  { placeholder:
                                      "Type field name here"
                                  , unfilled:
                                      "(empty field name)"
                                  , title:
                                      "Click to edit field name"
                                  , validator: validFieldName
                                  }))
                            (TextInput.Input
                               { text: key
                               , notThese:
                                   Set.fromFoldable (map field2Key fields)
                               })
                            (\name' ->
                               pure
                                 (TriggerUpdatePath
                                    (Shared.UpdatePath
                                       { path:
                                           path Shared.DataHere
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
                               { editor: Right editor'
                               , cells
                               , code: editorCode editor'
                               , path:
                                   path <<< Shared.DataFieldOf key
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
       fields)

--------------------------------------------------------------------------------
-- Errors

renderError :: forall t10 t11. View CellError -> HH.HTML t11 t10
renderError msg =
  HH.div
    [HP.class_ (HH.ClassName "error-message")]
    [ HH.text
         (caseCellError {
           "FillErrors" : \fillErrors ->
             let fromFillError =
                     caseFillError {
                       "NoSuchGlobal": \name -> "missing name “" <> name <> "”",
                       "OtherCellProblem": \name ->
                         "other cell “" <> name <> "” has a problem"
                      }
             in joinWith ", " (map fromFillError fillErrors),

           "CyclicCells": \ names ->
             "cells refer to eachother in a loop:" <> " " <>
             joinWith ", " names,
           "DuplicateCellName":  "this name is used twice",
           "CellRenameErrors": "internal bug; please report!", -- TODO:make this automatic.
           "CellTypeError": "types of values don't match up",
           "CellStepEror": "error while evaluating formula",
           "SyntaxError": "syntax error, did you mistype something?"
          } msg)
    ]

--------------------------------------------------------------------------------
-- Code regenerators

editorCode :: View Shared.Tree2 -> String
editorCode =
  caseTree2 {
    "MiscTree2":       \v original t ->
      originalOr original "",
    "TextTree2":       \v original t ->
      originalOr original "",
    "VegaTree2":       \v original t ->
      originalOr original "",
    "VariantTree2":    \v original tag arg ->
      originalOr original "",
    "ArrayTree2":      \v original editors ->
      originalOr original "",
    "RecordTree2":     \v original fields ->
      originalOr original "",
    "TableTreeMaybe2": \v original columns rows ->
      originalOr original "",
    "HoleTree": "_"
  }

originalOr :: View Shared.OriginalSource -> String -> String
originalOr v s = caseOriginalSource  {
   "NoOriginalSource": s,
   "OriginalSource": \s' -> s'
   } v

--------------------------------------------------------------------------------
-- Tree casing

caseTree2Default :: forall t628 t629 t630 t631 t632 t633 t634 t635 t636 t637 t638 t639 t640 t641 t642 t643 t644 t645 t646 t647 t648 t649 t650 t651.
  t648
  -> { "ArrayTree2" :: t641 -> t642 -> t643 -> t648
     , "HoleTree" :: t648
     , "MiscTree2" :: t628 -> t629 -> t630 -> t648
     , "RecordTree2" :: t638 -> t639 -> t640 -> t648
     , "TableTreeMaybe2" :: t644 -> t645 -> t646 -> t647 -> t648
     , "TextTree2" :: t631 -> t632 -> t633 -> t648
     , "VariantTree2" :: t634 -> t635 -> t636 -> t637 -> t648
     , "VegaTree2" :: t649 -> t650 -> t651 -> t648
     }
caseTree2Default d = {
    "MiscTree2":       \v _originalSource t -> d,
    "TextTree2":       \v _originalSource t -> d,
    "VariantTree2":    \v _originalSource tag arg -> d,
    "RecordTree2":     \v _originalSource fields -> d,
    "ArrayTree2":      \v _originalSource editors -> d,
    "TableTreeMaybe2":      \v _originalSource editors _ -> d,
    "VegaTree2":       \v _originalSource t -> d,
    "HoleTree": d
  }
