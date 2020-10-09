-- | A declaration in a document.

module Inflex.Components.Cell
  ( component
  , Pos(..)
  , Query(..)
  , Cell(..)
  , Output(..)
  ) where

import Data.Either (Either(..), either)
import Data.Int (round)
import Data.Map (Map)
import Data.Maybe (Maybe(..))

import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect (Effect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Inflex.Components.Cell.Editor as Editor
import Inflex.Components.Cell.Name as Name
import Inflex.Schema as Shared
import Prelude (Unit, bind, discard, identity, pure, show, unit, (-), (<<<), (<>), (>>=))
import Web.DOM.Node as Node
import Web.Event.Event (currentTarget)
import Web.HTML.Event.DragEvent as DE
import Web.HTML.HTMLElement as HTML
import Web.UIEvent.MouseEvent as ME

--------------------------------------------------------------------------------
-- Component types

type Input = Shared.OutputCell

type Pos = { x :: Int, y :: Int }

data Query a = SetXY Pos

data Output
  = CellUpdate { name :: String, code :: String}
  | RemoveCell
  | CellDragStart DE.DragEvent

data State = State
  { cell :: Cell
  , display :: Display
  , pos :: Maybe Pos
  , offset :: Maybe {x::Int,y::Int, innerx :: Int, innery :: Int}
  }

data Command
  = SetCell Cell
  | CodeUpdate Cell
  | DeleteCell
  | DragStarted DE.DragEvent
  | MouseDown ME.MouseEvent

--------------------------------------------------------------------------------
-- Internal types

data Cell = Cell
  { name :: String
  , code :: String
  , result :: Either Shared.CellError Editor.Editor
  }

data Display
  = DisplayResult
  | DisplayEditor String
  | DisplayTable (Set String) (Array (Map String String))

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadEffect m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\cell ->
           State
             { cell: outputCellToCell cell
             , display: DisplayResult
             , pos: Nothing
             , offset: Nothing
             })
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetCell <<< outputCellToCell
            , handleQuery = query
            }
    }

outputCellToCell :: Shared.OutputCell -> Cell
outputCellToCell (Shared.OutputCell {name, code, result}) =
  Cell
    { name
    , code
    , result:
        case result of
          Shared.ResultError e -> Left e
          Shared.ResultOk (Shared.ResultTree output) ->
            Right
              (case output of
                 Shared.MiscTree _ text -> Editor.MiscE text
                 Shared.ArrayTree _ _ -> Editor.MiscE "list here!")
    }

--------------------------------------------------------------------------------
-- Query

query ::
     forall a action slots output m. (MonadEffect m)
  => Query a
  -> H.HalogenM State action slots output m (Maybe a)
query =
  case _ of
    SetXY xy -> do
      State s <- H.get
      H.modify_
        (\(State s') ->
           State
             (s'
                { pos =
                    case s.offset of
                      Nothing -> Nothing
                      Just off -> Just {x: (xy . x) - off.x - off.innerx, y: (xy . y) - off.y - off.innery}
                }))
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

foreign import clearDragImage :: DE.DragEvent -> Effect Unit
foreign import setEmptyData :: DE.DragEvent -> Effect Unit

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    CodeUpdate (Cell {name, code}) -> do
      H.liftEffect (log "Inflex.Cell:CodeUpdate, raising ...")
      H.raise (CellUpdate {name, code})
    SetCell cell -> do
      H.modify_ (\(State s) -> State (s {cell = cell}))
    DeleteCell -> H.raise RemoveCell
    DragStarted dragEvent -> do
      H.liftEffect
        (do setEmptyData dragEvent
            clearDragImage dragEvent)
      H.raise (CellDragStart dragEvent)
    MouseDown mouseEvent -> do
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
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit, declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State {cell: Cell {name, code, result}, display, pos}) =
  HH.div
    [ HP.class_ (HH.ClassName "cell-wrapper")
    , HP.draggable true
    , HE.onDragStart (Just <<< DragStarted)
    , HE.onMouseDown (Just <<< MouseDown)
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
                Name.component
                name
                (\name' ->
                   pure
                     (CodeUpdate
                        (Cell {name: name', result, code})))
            , HH.button
                [ HP.class_ (HH.ClassName "delete-cell")
                , HE.onClick (\_ -> pure DeleteCell)
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
                   })
                (\code' ->
                   pure
                     (CodeUpdate
                        (Cell {name, result, code: code'})))
            ]
        ]
    ]
