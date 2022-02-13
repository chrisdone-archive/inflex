-- | Recursive editing of parts of a result.

module Inflex.Components.Cell.Editor
  ( Field(..)
  , Row(..)
  , component) where


-- import Data.Int
import Timed (timed)
import Data.Maybe (Maybe(..), isJust)
import Inflex.Frisson.Value (unViewValue)
import Effect.Class.Console (log)
import Inflex.Components.ProseMirror as Prose
import Inflex.Frisson (View, caseCellError, caseDataPath, caseFillError, caseMaybeRow, caseOriginalSource, caseTree2, caseTypeOf, caseVariantArgument, field2Key, field2Value, namedTypeName, namedTypeTyp, nestedCellErrorError, nestedCellErrorPath, rowFields)

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Nullable (Nullable, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith, trim, stripPrefix, Pattern(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Manage as Manage
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Inflex.Components.Cell.TextInput as TextInput
import Inflex.Components.Code as Code
import Inflex.FieldName (validFieldName)
import Inflex.Schema (CellError)
import Inflex.Schema as Shared
import Inflex.Types (OutputCell)
import Prelude (class Show, Unit, bind, const, discard, map, max, mempty, min, negate, pure, show, unit, (&&), (+), (-), (/=), (<<<), (<>), (==), (>), (>>=))
import Web.DOM.Element (Element, fromEventTarget)
import Web.Event.Event (preventDefault, stopPropagation, currentTarget)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
import Web.UIEvent.MouseEvent (toEvent)
import Web.UIEvent.WheelEvent (toEvent) as Wheel

import Inflex.Components.Cell.Editor.Types (EditorAndCode(..), Input, Output(..), Query(..))

--------------------------------------------------------------------------------
-- Component types

data State = State
  { display :: Display
  , editor :: Either (View Shared.CellError) (View Shared.Tree2)
  , code :: String
  , path :: Shared.DataPath -> Shared.DataPath
  , cellError :: Maybe (View CellError)
  , cells :: Map UUID (OutputCell)
  , tableOffset :: Int
  , rowCount :: Int
  , type' :: Maybe (View Shared.TypeOf)
  , tableMode :: TableMode
  }

data TableMode
  = NormalMode
  | EditingRows Editing

data Editing = Editing
  { rows :: Array Int -- Row index 0-based.
  }

data Command
  = SetEditorInput EditorAndCode
  | StartEditor
  | FinishEditing String
  | PreventDefault Event'
                   Command
  | AndThen Command Command
  -- | Autoresize Event
  | NoOp
  | SetInput String
  | InputElementChanged (ElemRef' Element)
  | VegaElementChanged String (ElemRef' Element)
  | TriggerUpdatePath (Maybe UUID) Shared.UpdatePath
  | ScrollTable Event'
  | ScrollIntegral Int Shared.DataPath Event'
  | ScrollToBottom
  | VariantDropdownChanged Shared.DataPath Event'
  | AddRowForEditing Int

derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show x = genericShow x

--------------------------------------------------------------------------------
-- Internal types

data Display
  = DisplayEditor
  | DisplayCode

newtype Event' = Event' Event
instance showEvent :: Show Event' where show _ = "Event"

newtype ElemRef' a = ElemRef' (ElemRef a)
instance showElemRef :: Show (ElemRef' a) where show _ = "ElemRef"

type Slots i =
  ( editor :: H.Slot i Output String
  , fieldname :: H.Slot i String String
  , textEditor :: H.Slot i String Unit
  , code :: H.Slot Code.Query Code.Output Unit
  , prosemirror :: H.Slot Prose.Query Prose.Output Unit
  )

data Row = Row { fields :: Array Field, original :: Shared.OriginalSource}
         | HoleRow (View Shared.Tree2)
derive instance genericRow :: Generic Row _
instance showRow :: Show Row where show x = genericShow x

newtype Field = Field { key :: String , value :: View Shared.Tree2}
derive instance genericField :: Generic Field _
instance showField :: Show Field where show x = genericShow x

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
        (\input@(EditorAndCode {editor, code, path, cells, type'}) ->
           State
             { display: DisplayEditor
             , editor
             , code
             , path
             , cellError: Nothing
             , cells
             , tableOffset: 0
             , rowCount: getRowCount editor
             , type'
             , tableMode: NormalMode
             })
    , render: \state -> timed "Editor.render" (\_ -> render state)
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
         , "ArrayTree2" = \_v _src rows ->
             Array.length rows
         })
         e

--------------------------------------------------------------------------------
-- Query

query :: forall t1162 t1166 t1167 t1171.
        MonadAff t1167 => Query t1162
                          -> H.HalogenM State t1166
                               (Slots Query)
                               Output
                               t1167
                               (Maybe t1171)
query =
  case _ of
    NewInput input -> do
      _ <- eval (SetEditorInput input)
      pure Nothing
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
    AddRowForEditing i ->
       H.modify_ (\(State s) -> State (s {tableMode = case s.tableMode of
                                              NormalMode -> EditingRows (Editing{rows:[i]})
                                              EditingRows (Editing{rows}) ->
                                                EditingRows (Editing{rows: if Array.elem i rows then Array.filter (_ /= i) rows else rows <> [i]})}))
    ScrollIntegral i path (Event' event) -> do
       delta <- H.liftEffect (getDeltaY event)
       eval' (TriggerUpdatePath
                        Nothing
                        (Shared.UpdatePath
                           { path
                           , update:
                               Shared.CodeUpdate
                                 (Shared.Code {text: show (i + (if delta > 0.0 then -1 else 1))})
                           }))
    VariantDropdownChanged path (Event' event) ->
      case currentTarget event of
        Nothing -> pure unit
        Just x ->
          case fromEventTarget x of
            Just htmlelement -> do
              mvalue <- H.liftEffect (getValue htmlelement)
              case toMaybe mvalue of
                Nothing -> pure unit
                Just text -> do
                  eval' (TriggerUpdatePath
                        Nothing
                        (Shared.UpdatePath
                           { path
                           , update:
                               Shared.CodeUpdate
                                 (Shared.Code {text})
                           }))
            Nothing -> pure unit
    TriggerUpdatePath muuid update -> H.raise (UpdatePath muuid update)
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
    SetEditorInput input@(EditorAndCode {editor, code, path, cells, type'}) -> do
      log ("SetEditorInput: " <> code)
      State state <- H.get
      H.put
        (State
           { path
           , editor
           , code
           , display: DisplayEditor
           , cellError: Nothing
           , cells
           , tableOffset: state.tableOffset
           , rowCount: getRowCount editor
           , type'
           , tableMode: NormalMode
           })
    ScrollTable (Event' e) -> do
      delta <- H.liftEffect (getDeltaY e)
      if delta > 0.0
        then H.modify_
               (\(State s) ->
                  State
                    (s
                       { tableOffset =
                           max 0 (min (s . rowCount - pageSize) (s . tableOffset + 1))
                       }))
        else H.modify_
               (\(State s) ->
                  State (s {tableOffset = max 0 (s . tableOffset - 1)}))
    ScrollToBottom ->
      H.modify_
               (\(State s) ->
                  State
                    (s
                       { tableOffset =
                           max 0 (min (s . rowCount - pageSize) (s . rowCount))
                       }))

    PreventDefault (Event' e) c -> do
      H.liftEffect
        (do preventDefault e
            stopPropagation e)
      eval' c
    NoOp -> pure unit
    AndThen x y -> do
       eval' x
       eval' y

foreign import getDeltaY :: Event -> Effect Number
foreign import getValue :: Element -> Effect (Nullable String)
foreign import setStyle :: String -> Element -> Effect Unit
foreign import autosize :: HTMLElement -> Effect Unit
foreign import vegaInPlace :: HTMLElement -> String -> Effect Unit
foreign import printText :: String -> String

--------------------------------------------------------------------------------
-- Render main component

render :: forall a. MonadAff a => State -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
render (State {display, code, editor, path, cellError, cells, tableOffset, type',tableMode}) =
  case display of
    DisplayCode -> wrapper (renderControl <> errorDisplay)
    DisplayEditor ->
      if blank code editor
        then wrapper (renderControl)
        else wrapper (case editor of
                        Left msg -> [renderError msg]
                        Right e -> renderEditor editing type' tableOffset path cells e)
  where
    editing = case tableMode of
       NormalMode -> []
       EditingRows (Editing{rows}) -> rows
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
          let boundaryWrap inner' =
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
                      inner'))
          in case editor of
             Left _ -> boundaryWrap inner
             Right e ->
               caseTree2
                  ((caseTree2Default
                   (boundaryWrap inner))
                   {
                    "HoleTree" = \originalSource ->
                      caseOriginalSource {
                        "NoOriginalSource": HH.div [] inner,
                        "OriginalSource": \_ -> HH.div
                        [ HP.class_
                            (HH.ClassName "editor-boundary-wrap clickable-to-edit")
                        , HP.title "Click to edit"
                        , HE.onClick
                            (\ev ->
                               pure (StartEditor))
                        ]
                        inner
                        } originalSource,
                    "MiscTree2" = \v originalSource t ->
                     caseOriginalSource {
                        "NoOriginalSource": HH.div [notEditable] inner,
                        "OriginalSource": \_ -> HH.div
                        [ HP.class_
                            (HH.ClassName "editor-boundary-wrap clickable-to-edit")
                        , HP.title "Click to edit"
                        -- TODO: Stop capture of ProseMirror.
                        , HE.onClick
                            (\ev ->
                               pure (PreventDefault (Event' (toEvent ev)) StartEditor))
                        ]
                        inner
                     } originalSource
                  })
                  e
    errorDisplay =
      case cellError of
        Nothing -> []
        Just error -> [renderError error]

notEditable :: forall t100 t101.
  HH.IProp
    ( class :: String
    | t101
    )
    t100
notEditable = HP.class_ (HH.ClassName "not-editable")

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
  => Array Int -> Maybe (View Shared.TypeOf)
  -> Int
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> View Shared.Tree2
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderEditor editing type' offset path cells =
  caseTree2 {
    "MiscTree2":       \v o t ->
      [HH.div [HP.class_ (HH.ClassName "misc")
          -- Commenting out clever scrolling trick for now
          -- HE.onWheel (\e -> case fromString t  of
          --                     Nothing -> Nothing
          --                     Just i -> Just (PreventDefault
          --                         (Event' (Wheel.toEvent e))
          --                         (ScrollIntegral i
          --                                         (path Shared.DataHere) (Event' (Wheel.toEvent e)))))
       ] [HH.text t]],
    "TextTree2":       \v originalSource t ->
      [renderTextEditor originalSource path t],
    "VegaTree2":       \v _originalSource t ->
      [renderVegaEditor path t],
    "VariantTree2":    \v originalSource tag arg ->
      [renderVariantEditor type' originalSource path cells tag arg],
    "ArrayTree2":      \v originalSource editors ->
      renderArrayEditor editing type' originalSource path cells offset editors,
    "RecordTree2":     \v originalSource fields ->
      [renderRecordEditor (rowType type') originalSource path cells fields],
    "TableTreeMaybe2": \v originalSource columns rows ->
      renderTableEditor editing type' originalSource offset path cells columns rows,
    -- TODO: produce an empty spine when in certain contexts -- e.g. text fields in a table.
    "HoleTree": \originalSource ->
      -- if hasOriginalSource originalSource then
          materialize type' originalSource offset path cells,
       -- else holeFallback
    "DocTree2": \originalSource doc ->
      [renderRichEditor type' path cells doc]
  }

--------------------------------------------------------------------------------
-- Rich editor

renderRichEditor ::
     forall a. MonadAff a
  => Maybe (View Shared.TypeOf)
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> View Shared.Value
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderRichEditor mtype mkPath cells json =
  HH.slot
    (SProxy :: SProxy "prosemirror")
    unit
    (H.hoist H.liftAff (Prose.component component))
    { cells: cells, the_json: unViewValue json }
    (case _ of
        Prose.UpdatePath muuid update -> Just (TriggerUpdatePath muuid update)
        Prose.TextOutput code ->
          pure
            (TriggerUpdatePath
               Nothing
               (Shared.UpdatePath
                  { path: mkPath Shared.DataHere
                  , update:
                      Shared.CodeUpdate
                        (Shared.Code {text: code})
                  })))

--------------------------------------------------------------------------------
-- Variant display

renderVariantEditor ::
     forall a. MonadAff a
  => Maybe (View Shared.TypeOf)
  -> View Shared.OriginalSource
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> String
  -> View Shared.VariantArgument
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderVariantEditor type' originalSource path cells tag marg =
  HH.div
    [HP.class_ (HH.ClassName "variant")]
    ([HH.div [HP.class_ (HH.ClassName "variant-tag")]
      [case type' of

                    Just t | hasOriginalSource originalSource -> caseTypeOf {
                        "ArrayOf": const (HH.text ""),
                        "MiscType": (HH.text "")
                        ,"RecordOf": const (HH.text ""),
                        "TableOf": const (HH.text ""),
                        "TextOf": (HH.text ""),
                        -- TODO:
                        "VariantOf": \types open ->
                          -- show types <> " -- " <> show open
                           case types of
                             [_] -> HH.text ("#" <> tag)
                             _ -> HH.select [HE.onChange (\e -> Just (VariantDropdownChanged
                                                              (path Shared.DataHere)
                                                              (Event' e)))]
                                   (map (\typ -> HH.option [HP.value ("#" <> namedTypeName typ <>
                                                       if isEmptyRecordType (namedTypeTyp typ)
                                                           then "" else "(_)"
                                                          ),
                                                     HP.selected (namedTypeName typ == tag)] [HH.text ("#" <> namedTypeName typ)])
                                types)
                        } t
                    _ -> HH.text ("#" <> tag) -- TODO: check should never happen
         ]] <>
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
               , type': do
                  types' <- variantType type'
                  namedType <- Array.find (\namedType -> namedTypeName namedType == tag) types'
                  pure (namedTypeTyp namedType)
               })
            (\output ->
               case output of
                 UpdatePath muid update -> Just (TriggerUpdatePath muid update)
                 NewCode rhs ->
                   Just
                     (TriggerUpdatePath
                        Nothing
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
               , Manage.manage (VegaElementChanged vegaSpec <<< ElemRef')
               ]
               [])
    ]

--------------------------------------------------------------------------------
-- Text editor

renderTextEditor ::
     forall i a. MonadAff a
  => View Shared.OriginalSource -> (Shared.DataPath -> Shared.DataPath)
  -> String
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots i) a Command) Command
renderTextEditor originalSource path text =
  if hasOriginalSource originalSource
    then HH.slot
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
                   Nothing
                   (Shared.UpdatePath
                      { path: path Shared.DataHere
                      , update:
                          Shared.CodeUpdate
                            (Shared.Code {text: printText text'})
                      })))
    else HH.div [HP.class_ (HH.ClassName "text not-editable")] [HH.text text]

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
  => Array Int
  -> Maybe (View Shared.TypeOf)
  -> View Shared.OriginalSource
  -> Int
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Array String
  -> Array (View Shared.MaybeRow)
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderTableEditor editing type0 originalSource offset path cells columns rows =
  [ listMenu editing path
  , HH.table
      [HP.class_ (HH.ClassName ("table " <> if hasOriginalSource' then "" else "not-editable"))]
      [ tableHeading originalSource path columns emptyTable
      , HH.tbody
          [HP.class_ (HH.ClassName "table-body"), HE.onWheel (\e -> Just (PreventDefault
                                                                   (Event' (Wheel.toEvent e))
                                                                   (ScrollTable (Event' (Wheel.toEvent e)))))]
          (bodyGuide hasOriginalSource' emptyTable emptyRows <>
           mapWithIndexNarrow offset pageSize (\i -> caseMaybeRow {
             "SomeRow": tableRow editing (rowType type') hasOriginalSource' columns path cells i
            ,"HoleRow": tableRowHole editing (rowType type') hasOriginalSource' columns path cells i
           }) rows <>
           if hasOriginalSource' then addNewRow else [HH.tr[][HH.td[HP.class_ (HH.ClassName "add-row")][],stats]])
      ]
  ]
  where
    type' = arrayType type0
    hasOriginalSource' = hasOriginalSource originalSource
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
                                     (AndThen

                                        (TriggerUpdatePath
                                        Nothing
                                        (Shared.UpdatePath
                                           { path:
                                               path Shared.DataHere
                                           , update:
                                               Shared.AddToEndUpdate
                                           }))
                                     (ScrollToBottom))))
                  ]
                  [HH.text "+"]
              ]
          , stats
          ]
      ]
    stats =
        HH.td
              [ HP.class_ (HH.ClassName "bottom-blank")
              , HP.colSpan
                  (Array.length columns + 1 +
                   (if emptyTable
                      then 1
                      else 0))
              ]
              (if len>1 then [HH.text ((if len>pageSize then show (1+offset) <> "-" <> show (min (offset+pageSize) len) <> " of " else "") <>
                        show (len) <> " rows")] else
                 [])
    disabled = Array.null columns
    fieldset = Set.fromFoldable columns
    len = Array.length rows

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
  => Array Int -> Maybe (Array (View Shared.NamedType)) -> Boolean -> Array String
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Int
  -> View Shared.Tree2
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
tableRowHole editing types editable columns path cells rowIndex editor' =
  HH.tr
    [HP.class_ (HH.ClassName (if Array.elem rowIndex editing then "row row-editing" else "row"))]
    ([rowNumber editable rowIndex path] <>
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
                       , type': do
                           types' <- types
                           namedType <- Array.find (\namedType -> namedTypeName namedType == key) types'
                           pure (namedTypeTyp namedType)
                       , cells
                       , path:
                           path <<<
                           Shared.DataElemOf rowIndex <<<
                           Shared.DataFieldOf key
                       })
                    (\output ->
                       case output of
                         UpdatePath muid update -> Just (TriggerUpdatePath muid update)
                         NewCode rhs ->
                           Just
                             (TriggerUpdatePath
                                Nothing
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
  => Array Int -> Maybe (Array (View Shared.NamedType))
  -> Boolean
  -> Array String
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Int
  -> View Shared.Row
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
tableRow editing types editable columns path cells rowIndex row =
  HH.tr
    [HP.class_ (HH.ClassName (if Array.elem rowIndex editing then "row row-editing" else "row"))]
    ([rowNumber editable rowIndex path] <>
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
                       , type': do
                           types' <- types
                           namedType <- Array.find (\namedType -> namedTypeName namedType == key) types'
                           pure (namedTypeTyp namedType)
                       , path:
                           path <<<
                           Shared.DataElemOf rowIndex <<< Shared.DataFieldOf key
                       })
                    (\output ->
                       case output of
                         UpdatePath muid update -> Just (TriggerUpdatePath muid update)
                         NewCode rhs ->
                           Just
                             (TriggerUpdatePath
                                Nothing
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
     (if editable then [addColumnBlank] else []))
  where
    addColumnBlank = HH.td [HP.class_ (HH.ClassName "add-column-blank")] []

rowNumber :: forall t19. Boolean -> Int -> (Shared.DataPath -> Shared.DataPath) -> HH.HTML t19 Command
rowNumber editable rowIndex path =
  HH.td
    [ HP.class_ (HH.ClassName ("row-number"))
    , HE.onClick
        (\e ->
           if editable
             then pure
                    (PreventDefault
                       (Event' (toEvent e))
                       (AddRowForEditing rowIndex))
             else Nothing)
    ]
    [ HH.div
        [HP.class_ (HH.ClassName "row-number-div")]
        [ HH.div
            [HP.class_ (HH.ClassName "row-number-text")]
            [HH.text (show (rowIndex + 1))]
        ]
    ]

listMenu :: forall t196. Array Int -> (Shared.DataPath -> Shared.DataPath) -> HH.HTML t196 Command
listMenu editing path =
  case editing of
    [] -> HH.text ""
    _ ->
      HH.div
        [HP.class_ (HH.ClassName "table-edit-menu")]
        [ HH.button
            [ HP.class_ (HH.ClassName "menu-button")
            , HE.onClick
                (\e ->
                   pure
                     (PreventDefault
                        (Event' (toEvent e))
                        (TriggerUpdatePath
                           Nothing
                           (Shared.UpdatePath
                              { path: path Shared.DataHere
                              , update:
                                  Shared.RemoveUpdate
                                    (Shared.Removals
                                       {indices: editing})
                              }))))
            ]
            [HH.text "Delete"]
        , HH.button
            [ HP.class_ (HH.ClassName "menu-button")
            , HE.onClick
                (\e ->
                   pure
                     (PreventDefault
                        (Event' (toEvent e))
                        (TriggerUpdatePath
                           Nothing
                           (Shared.UpdatePath
                              { path: path Shared.DataHere
                              , update:
                                  Shared.MoveUpdate
                                    (Shared.Removals
                                       {indices: editing})
                                    (-1)
                              }))))
            ]
            [HH.text "Up"]
        , HH.button
            [ HP.class_ (HH.ClassName "menu-button")
            , HE.onClick
                (\e ->
                   pure
                     (PreventDefault
                        (Event' (toEvent e))
                        (TriggerUpdatePath
                           Nothing
                           (Shared.UpdatePath
                              { path: path Shared.DataHere
                              , update:
                                  Shared.MoveUpdate
                                    (Shared.Removals
                                       {indices: editing})
                                    1
                              }))))
            ]
            [HH.text "Down"]
        ]

bodyGuide :: forall t651 t652. Boolean -> Boolean -> Boolean -> Array (HH.HTML t652 t651)
bodyGuide hasOriginalSource' emptyTable emptyRows =
  if emptyTable && hasOriginalSource'
    then [ HH.tr
             []
             [ HH.td
                 [HP.colSpan 3, HP.class_ (HH.ClassName "table-empty")]
                 [HH.text "Hit the top-right button to add columns! ↗"]
             ]
         ]
    else if emptyRows && hasOriginalSource'
           then [ HH.tr
                    []
                    [ HH.td
                        [HP.colSpan 3, HP.class_ (HH.ClassName "table-empty")]
                        [HH.text "↙ Hit the bottom-left button to add rows!"]
                    ]
                ]
           else if emptyTable && emptyRows then
            [ HH.tr
                    []
                    [ HH.td
                        [HP.colSpan 3, HP.class_ (HH.ClassName "table-empty")]
                        [HH.text "(Empty table)"]
                    ]
                ] else []

tableHeading :: forall t627 t628 t629.
   MonadAff t628
  => View Shared.OriginalSource -> (Shared.DataPath -> Shared.DataPath)
  -> Array String
  -> Boolean
  -> HH.HTML (H.ComponentSlot HH.HTML ( fieldname :: H.Slot t627 String String | t629) t628 Command) Command
tableHeading originalSource path columns emptyTable =
  HH.thead
    [HP.class_ (HH.ClassName "table-header")]
    ([HH.th [HP.class_ (HH.ClassName "table-column"), HP.title ""] []] <>
     mapWithIndex
       (\i text ->
          HH.th
            (if hasOriginalSource originalSource
               then [ HP.class_ (HH.ClassName "table-column")
                    , HP.title "Click to edit"
                    ]
               else [HP.class_ (HH.ClassName "not-editable")])
            [ HH.div
                [HP.class_ (HH.ClassName "table-column-content")]
                (if hasOriginalSource originalSource
                   then [ columnNameSlot (Set.fromFoldable columns) path i text
                        , removeColumnButton path text
                        ]
                   else [ HH.div
                            [HP.class_ (HH.ClassName "cell-name")]
                            [HH.text text]
                        ])
            ])
       columns <>
     (if emptyTable
        then [HH.th [] []]
        else []) <>
     (if hasOriginalSource originalSource
        then [newColumnButton path columns]
        else []))

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
            Nothing
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
                   Nothing
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
                       Nothing
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
  => Array Int -> Maybe ( (View Shared.TypeOf)) ->
     View Shared.OriginalSource -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
     -> Int
  -> Array (View Shared.Tree2)
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
renderArrayEditor editing type' originalSource path cells offset editors =
  [ listMenu editing path,
  HH.table
    [HP.class_ (HH.ClassName ("array " <> if hasOriginalSource' then "" else "not-editable"))]
    [HH.tbody [HP.class_ (HH.ClassName "array-body"),
    HE.onWheel (\e -> Just (PreventDefault
                                                                   (Event' (Wheel.toEvent e))
                                                                   (ScrollTable (Event' (Wheel.toEvent e)))))
    ] (body <> addNewRow)]]
  where
    body =
      case editors of
        [] ->
          [ HH.tr
              []
              [ HH.td
                  [HP.colSpan 3, HP.class_ (HH.ClassName "array-empty")]
                  [HH.text (if hasOriginalSource'
                               then "↙ Hit the bottom-left button to add rows!"
                               else "(Empty list)")]
              ]
          ]
        _ -> rows
    rows =
      mapWithIndexNarrow offset pageSize
        (\i editor' ->
           let childPath = path <<< Shared.DataElemOf i
            in HH.tr
                 [HP.class_ (HH.ClassName (if Array.elem i editing then "row row-editing" else "row"))]
                 [ rowNumber hasOriginalSource' i path
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
                            , type': arrayType type'
                            })
                         (\output ->
                            case output of
                              UpdatePath muuid update ->
                                Just (TriggerUpdatePath muuid update)
                              NewCode rhs ->
                                Just
                                  (TriggerUpdatePath
                                     Nothing
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
    hasOriginalSource' = hasOriginalSource originalSource
    addNewRow =
      if hasOriginalSource'
        then [ HH.tr
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
                                     (AndThen (TriggerUpdatePath
                                        Nothing
                                        (Shared.UpdatePath
                                           { path:
                                               path Shared.DataHere
                                           , update:
                                               Shared.AddToEndUpdate
                                           })) ScrollToBottom)))
                         ]
                         [HH.text "+"]
                     ]
                 , HH.td [HP.class_ (HH.ClassName "bottom-blank")]
                     (if len>1 then [HH.text ((if len>pageSize then show (1+offset) <> "-" <> show (min (offset+pageSize) len) <> " of " else "") <>
                        show (len) <> " rows")] else
                 [])
                 ]
             ]
        else [ HH.tr
                 []
                 [ HH.td
                     [HP.class_ (HH.ClassName "add-row")]
                     [
                     ]
                 -- Copy/paste of the above. You can factor it out.
                 , HH.td [HP.class_ (HH.ClassName "bottom-blank")]
                     (if len>1 then [HH.text ((if len>pageSize then show (1+offset) <> "-" <> show (min (offset+pageSize) len) <> " of " else "") <>
                        show (len) <> " rows")] else
                 [])
                 ]
             ]
    len = Array.length editors

--------------------------------------------------------------------------------
-- Records

renderRecordEditor ::
     forall a. MonadAff a
  => Maybe (Array (View Shared.NamedType))
  -> View Shared.OriginalSource
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Array (View Shared.Field2)
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
renderRecordEditor types originalSource path cells fields =
  HH.table
    [HP.class_ (HH.ClassName "record")]
    ((if hasOriginalSource' then [ HH.button
          [ HP.class_ (HH.ClassName "add-field-button")
          , HE.onClick
              (\e ->
                 pure
                   (PreventDefault
                      (Event' (toEvent e))
                      (TriggerUpdatePath
                         Nothing
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
      ] else []) <>
     map
       (\field ->
          let key = field2Key field
              editor' = field2Value field
           in let childPath = path <<< Shared.DataFieldOf key
               in HH.tr
                    [HP.class_ (HH.ClassName "record-field")]
                    [ HH.td
                        [HP.class_ (HH.ClassName "record-field-name")]
                        [ if hasOriginalSource'
                             then HH.button
                                    [ HP.class_ (HH.ClassName "remove-field-button")
                                    , HE.onClick
                                        (\e ->
                                           pure
                                             (PreventDefault
                                                (Event' (toEvent e))
                                                (TriggerUpdatePath
                                                   Nothing
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
                                    [HH.text "×"]
                             else HH.text ""
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
                                    Nothing
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
                               , type': do
                                   types' <- types
                                   namedType <- Array.find (\namedType -> namedTypeName namedType == key) types'
                                   pure (namedTypeTyp namedType)
                               })
                            (\output ->
                               case output of
                                 UpdatePath muid update ->
                                   Just (TriggerUpdatePath muid update)
                                 NewCode rhs ->
                                   Just
                                     (TriggerUpdatePath
                                        Nothing
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
  where hasOriginalSource' = hasOriginalSource originalSource


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
    "HoleTree": \original -> originalOr original "_",
    "DocTree2": \original _j -> originalOr original ""
  }

originalOr :: View Shared.OriginalSource -> String -> String
originalOr v s = caseOriginalSource  {
   "NoOriginalSource": s,
   "OriginalSource": \s' -> s'
   } v

hasOriginalSource :: View Shared.OriginalSource -> Boolean
hasOriginalSource = caseOriginalSource  {
   "NoOriginalSource": false,
   "OriginalSource": \_ -> true
   }

--------------------------------------------------------------------------------
-- Tree casing

caseTree2Default d = {
    "MiscTree2":       \v _originalSource t -> d,
    "TextTree2":       \v _originalSource t -> d,
    "VariantTree2":    \v _originalSource tag arg -> d,
    "RecordTree2":     \v _originalSource fields -> d,
    "ArrayTree2":      \v _originalSource editors -> d,
    "TableTreeMaybe2":      \v _originalSource editors _ -> d,
    "VegaTree2":       \v _originalSource t -> d,
    "HoleTree": \_originalSource -> d,
    "DocTree2": \_originalSource x -> d
  }

rowType :: Maybe (View Shared.TypeOf) -> Maybe (Array (View Shared.NamedType))
rowType Nothing = Nothing
rowType (Just t) = caseTypeOf {
  "ArrayOf": const Nothing,
  "MiscType": Nothing, "TextOf": Nothing
  ,"RecordOf": Just,
  "TableOf": const Nothing,
  "VariantOf": \_ _ -> Nothing
  } t

variantType :: Maybe (View Shared.TypeOf) -> Maybe (Array (View Shared.NamedType))
variantType Nothing = Nothing
variantType (Just t) = caseTypeOf {
  "ArrayOf": const Nothing,
  "MiscType": Nothing,
  "TextOf": Nothing,
  "RecordOf": const Nothing,
  "TableOf": const Nothing,
  "VariantOf": \fields _ -> Just fields
  } t

arrayType :: Maybe (View Shared.TypeOf) -> Maybe (View Shared.TypeOf)
arrayType type' = type' >>= (caseTypeOf {
                                "ArrayOf": Just,
                                "MiscType": Nothing,
                                "TextOf": Nothing
                                ,"RecordOf": const Nothing,
                                "TableOf": const Nothing,
                                "VariantOf": \_ _ -> Nothing
                                })

isEmptyRecordType :: View Shared.TypeOf -> Boolean
isEmptyRecordType = caseTypeOf {
  "ArrayOf": const false,
  "MiscType": false,"TextOf": false
  ,"RecordOf":  (case _ of [] -> true
                           _ -> false),
  "TableOf": const false,
  "VariantOf": \_ _ -> false
  }

--------------------------------------------------------------------------------
-- Materializing editors

materialize ::
     forall a. MonadAff a
  => Maybe (View Shared.TypeOf) -> View Shared.OriginalSource
  -> Int
  -> (Shared.DataPath -> Shared.DataPath)
  -> Map UUID (OutputCell)
  -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
materialize Nothing _ _ _ _ = holeFallback "no type"
materialize (Just type') originalSource offset path cells =
  caseTypeOf {
  "ArrayOf": const (holeFallback "ArrayOf"),
  "MiscType": holeFallback "msc"
  ,"RecordOf": const (holeFallback "rec"),
  "TableOf": const (holeFallback "table"),
  "VariantOf": \_ _ -> holeFallback "variant",
  "TextOf": [renderTextEditor originalSource path ""]
  } type'

holeFallback :: forall a. String -> Array (HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command)
holeFallback suffix = [HH.div [HP.class_ (HH.ClassName "hole"), HP.title "A blank hole"] [HH.text ("_")]]
