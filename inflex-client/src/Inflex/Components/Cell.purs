-- | A declaration in a document.

module Inflex.Components.Cell
  ( component
  , Cell(..)
  , Output(..)
  ) where

import Data.Either (Either(..), either)
import Data.Int
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Inflex.Components.Cell.Editor as Editor
import Inflex.Components.Cell.Name as Name
import Inflex.Schema as Shared
import Prelude
import Web.DOM.Element (Element, fromEventTarget)
import Web.Event.Event (stopPropagation)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement
import Web.HTML.HTMLElement (fromElement)

--------------------------------------------------------------------------------
-- Component types

type Input = Shared.OutputCell

data Output
  = CellUpdate { name :: String, code :: String}
  | RemoveCell

data State = State
  { cell :: Cell
  , display :: Display
  , element :: Maybe HTMLElement
  , offset :: Maybe Offset
  }

data Offset = Offset Int Int

data Command
  = SetCell Cell
  | CodeUpdate Cell
  | ApplyOffset Int Int
  | DeleteCell
  | StopPropagation Event
                    Command
  | CellElementChanged (ElemRef Element)

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

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState:
        (\cell ->
           State
             { cell: outputCellToCell cell
             , display: DisplayResult
             , element: Nothing
             , offset: Nothing
             })
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetCell <<< outputCellToCell
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
          Shared.ResultOk output -> Right (Editor.MiscE output)
    }

--------------------------------------------------------------------------------
-- Eval

eval :: forall q i m. MonadEffect m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    StopPropagation e c -> do
      H.liftEffect (stopPropagation e)
      eval c
    CodeUpdate (Cell {name, code}) -> do
      H.liftEffect (log "Inflex.Cell:CodeUpdate, raising ...")
      H.raise (CellUpdate {name, code})
    SetCell cell ->
      H.put
        (State
           { cell
           , display: DisplayResult
           , element: Nothing
           , offset: Nothing
           })
    DeleteCell -> H.raise RemoveCell
    ApplyOffset xoff yoff -> do
      State s <- H.get
      case s . element of
        Nothing -> pure unit
        Just htmlelement -> do

          l <- H.liftEffect $ do offsetLeft htmlelement
          t <- H.liftEffect $ do offsetTop htmlelement
          -- rect <- H.liftEffect $ getBoundingClientRect htmlelement
          -- let l = rect.left
          --     t= rect.top
          H.liftEffect $ log ("l,t=" <> show l <> "," <> show t)
          -- H.liftEffect $ log ("l',t'=" <> show (rect.left) <> "," <> show (rect.top))
          H.modify_
            (\(State s') ->
               State (s' {offset = Just (Offset (round l + xoff) (round t + yoff))}))
      H.liftEffect
        (log ("New offset:" <> show xoff <> "," <> show yoff))
    CellElementChanged elemRef ->
      case elemRef of
        Created element ->
          case fromElement element of
            Just htmlelement -> do
              H.modify_ (\(State s) -> State (s {element = Just htmlelement}))
            Nothing -> pure unit
        Removed _ -> pure unit

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadEffect m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot q String Unit, declname :: H.Slot q Name.Output Unit | keys) m Command)
                  Command
render (State {cell: Cell {name, code, result}, display, offset}) =
  HH.div
    [ HP.class_ (HH.ClassName "cell")
    , Name.manage CellElementChanged
    , HP.attr
        (H.AttrName "style")
        (case offset of
           Nothing -> ""
           Just (Offset x y) -> "left:" <> show x <> "px; top:" <> show y <> "px;position: absolute;")
    ]
    [ HH.div
        [HP.class_ (HH.ClassName "cell-header")]
        [ HH.slot
            (SProxy :: SProxy "declname")
            unit
            Name.component
            name
            (\output ->
               pure
                 (case output of
                    Name.NewName name' ->
                      CodeUpdate
                        (Cell {name: name', result, code})
                    Name.NewOffset x y -> ApplyOffset x y))
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
                 (CodeUpdate (Cell {name, result, code: code'})))
        ]
    ]
