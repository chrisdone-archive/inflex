-- | A declaration in a document.

module Inflex.Components.Cell
  ( component
  , Pos(..)
  , Query(..)
  , Cell(..)
  , Output(..)
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Either (Either(..), either)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell.Editor as Editor
import Inflex.Components.Cell.TextInput as TextInput
import Inflex.Schema as Shared
import Inflex.FieldName (validFieldName)
import Prelude
import Web.DOM.Node as Node
import Web.Event.Event (currentTarget)
import Web.HTML.Event.DragEvent as DE
import Web.HTML.HTMLElement as HTML
import Web.UIEvent.MouseEvent as ME

--------------------------------------------------------------------------------
-- Component types

type Input = Shared.OutputCell

type Pos = { x :: Int, y :: Int }

data Query a
  = SetXY Pos
  | NestedCellError Shared.NestedCellError

data Output
  = CellUpdate { name :: String, code :: String}
  | RemoveCell
  | CellDragStart DE.DragEvent
  | UpdatePath Shared.UpdatePath

data State = State
  { cell :: Cell
  , pos :: Maybe Pos
  , offset :: Maybe {x::Int,y::Int, innerx :: Int, innery :: Int}
  }

data Command
  = SetCellFromInput Cell
  | CodeUpdate Cell
  | DeleteCell
  | DragStarted DragEvent'
  | MouseDown MouseEvent'
  | TriggerUpdatePath Shared.UpdatePath

newtype DragEvent' = DragEvent' DE.DragEvent
derive instance genericDragEvent :: Generic DragEvent' _
instance showDragEvent :: Show DragEvent' where show _ = "DragEvent"

newtype MouseEvent' = MouseEvent' ME.MouseEvent
derive instance genericMouseEvent :: Generic MouseEvent' _
instance showMouseEvent :: Show MouseEvent' where show _ = "MouseEvent"

derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show x = genericShow x

--------------------------------------------------------------------------------
-- Internal types

data Cell = Cell
  { name :: String
  , code :: String
  , result :: Either Shared.CellError Editor.Editor
  , hash :: String
  }

derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where show x = genericShow x

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadEffect m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\cell ->
           State
             { cell: outputCellToCell cell

             , pos: Nothing
             , offset: Nothing
             })
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetCellFromInput <<< outputCellToCell
            , handleQuery = query
            }
    }

outputCellToCell :: Shared.OutputCell -> Cell
outputCellToCell (Shared.OutputCell {name, code, result, hash: Shared.Hash hash}) =
  Cell
    { name
    , code
    , hash
    , result:
        case result of
          Shared.ResultError e -> Left e
          Shared.ResultOk (Shared.ResultTree output) -> Right (toEditor output)
    }

toEditor :: Shared.Tree2 -> Editor.Editor
toEditor =
  case _ of
    Shared.MiscTree2 _ originalSource text ->
      Editor.MiscE originalSource text
    Shared.TextTree2 _ originalSource text ->
      Editor.TextE originalSource text
    Shared.VegaTree2 _ originalSource vega ->
      Editor.VegaE originalSource vega
    Shared.ArrayTree2 _ originalSource trees ->
      Editor.ArrayE originalSource  (map toEditor trees)
    Shared.VariantTree2 _ originalSource label margument ->
      Editor.VariantE
        originalSource
        label
        (case margument of
         Shared.VariantArgument tree -> Just (toEditor tree)
         _ -> Nothing)
    Shared.RecordTree2 _ originalSource fields ->
      Editor.RecordE originalSource
        (map (\(Shared.Field2{key,value}) -> Editor.Field ({key,value: toEditor value})) fields)
    Shared.TableTree2 _ originalSource columns rows ->
      Editor.TableE
        originalSource
        columns
        (map (\(Shared.Row {source, fields}) ->
                Editor.Row
                { original: source
                , fields:
                    map (\(Shared.Field2{key,value}) -> Editor.Field {key,value: toEditor value})
                        fields
                })
             rows)
    Shared.TableTreeMaybe2 _ originalSource columns rows ->
      Editor.TableE
        originalSource
        columns
        (map (\mrow ->
                case mrow of
                  Shared.HoleRow -> Editor.HoleRow
                  Shared.SomeRow (Shared.Row {source, fields}) -> Editor.Row
                        { original: source
                        , fields:
                            map (\(Shared.Field2{key,value}) -> Editor.Field {key,value: toEditor value})
                                fields
                        })
             rows)

--------------------------------------------------------------------------------
-- Query

query ::
     forall a action output m t0 t1 x. Ord t1 => (MonadEffect m)
  => Query a
  -> H.HalogenM State action (editor :: H.Slot Editor.Query t0 t1 | x) output m (Maybe a)
query =
  case _ of
    NestedCellError cellError -> do
      log ("[Cell] Received error:" <> show cellError)
      _ <- H.queryAll (SProxy :: SProxy "editor")
                 (Editor.NestedCellError cellError)
      pure Nothing
    SetXY xy -> do
      State s <- H.get
      H.modify_
        (\(State s') ->
           State
             (s'
                { pos =
                    case s . offset of
                      Nothing -> Nothing
                      Just off ->
                        Just
                          { x: (xy . x) - off . x - off . innerx
                          , y: (xy . y) - off . y - off . innery
                          }
                }))
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

foreign import clearDragImage :: DE.DragEvent -> Effect Unit
foreign import setEmptyData :: DE.DragEvent -> Effect Unit

-- eval' :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
-- eval' cmd = do
--   log (show cmd)
--   eval cmd

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    TriggerUpdatePath update -> H.raise (UpdatePath update)
    CodeUpdate (Cell {name, code}) -> do
      H.raise (CellUpdate {name, code})
    SetCellFromInput cell@(Cell {hash, name}) -> do
      when
        false
        (do State s0 <- H.get
            let Cell cell0 = s0 . cell
            if cell0 . hash == hash
              then log ("Ignoring unchanged cell " <> name)
              else log ("Updating changed cell " <> name))
      H.modify_ (\(State s) -> State (s {cell = cell}))
    DeleteCell -> H.raise RemoveCell
    DragStarted (DragEvent' dragEvent) -> do
      H.liftEffect
        (do setEmptyData dragEvent
            clearDragImage dragEvent)
      H.raise (CellDragStart dragEvent)
    MouseDown (MouseEvent' mouseEvent) -> do
      case currentTarget (ME.toEvent mouseEvent) >>= HTML.fromEventTarget of
        Nothing -> pure unit
        Just el -> do
          myRect <- H.liftEffect (HTML.getBoundingClientRect el)
          let xcoord = ME.clientX mouseEvent - round (myRect . left)
              ycoord = ME.clientY mouseEvent - round (myRect . top)
          mparent <- H.liftEffect (Node.parentElement (HTML.toNode el))
          case mparent >>= HTML.fromElement of
            Nothing -> pure unit
            Just parent -> do
              rect <- H.liftEffect (HTML.getBoundingClientRect parent)
              H.modify_
                (\(State s) ->
                   State
                     (s
                        { offset =
                            Just
                              { x: round (rect . left)
                              , y: round (rect . top)
                              , innerx: xcoord
                              , innery: ycoord
                              }
                        }))

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot Editor.Query Editor.Output Unit,
                                             declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State {cell: Cell {name, code, result, hash}, pos}) =
  HH.div
    [ HP.class_ (HH.ClassName "cell-wrapper")
    -- Disabling dragging for now
    -- , HP.draggable true
    -- , HE.onDragStart (Just <<< DragStarted <<< DragEvent')
    -- , HE.onMouseDown (Just <<< MouseDown <<< MouseEvent')
    , HP.prop
        (H.PropName "style")
        (case pos of
           Nothing -> ""
           Just p ->
             "left:" <> show (p . x) <> "px; top:" <>
             show (p . y) <>
             "px; position:absolute")
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "cell")
        ]
        [ HH.div
            [HP.class_ (HH.ClassName "cell-header")]
            [ HH.slot
                (SProxy :: SProxy "declname")
                unit
                (TextInput.component
                  (TextInput.Config
                     { placeholder: "Type a name here"
                     , unfilled: "(unnamed)"
                     , title: "Click to edit cell's name"
                     , validator: validFieldName
                     }))
                (TextInput.Input {text: name, notThese: mempty})
                (\name' ->
                   pure
                     (CodeUpdate
                        (Cell {name: name', result, code, hash})))
            , HH.button

                [ HP.class_ (HH.ClassName "delete-cell")
                , HE.onClick (\_ -> pure DeleteCell)
                , HP.title "Delete this cell"
                ]
                [HH.text "×"]
            ]
        , HH.div
            [HP.class_ (HH.ClassName "cell-body")]
            [ HH.slot
                (SProxy :: SProxy "editor")
                unit
                Editor.component
                (Editor.EditorAndCode
                   { editor: either Editor.ErrorE identity result
                   , code: code
                   , path: identity
                   })
                (\output ->
                  case output of
                    Editor.UpdatePath update -> Just (TriggerUpdatePath update)
                    Editor.NewCode code' ->
                     pure
                     (CodeUpdate
                        (Cell {name, result, code: code', hash})))
            ]
        ]
    ]
