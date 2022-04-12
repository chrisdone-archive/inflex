-- | A declaration in a document.
module Inflex.Components.Cell
  ( component
  , Input(..)
  , Query(..)
  , Cell(..)
  , Output(..)
  ) where

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.UUID (UUID(..))
import Dragger as Dragger
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Manage as Manage
import Halogen.Query.EventSource (effectEventSource, emit) as Src
import Inflex.Components.Cell.Editor as Editor
import Inflex.Components.Cell.Editor.Types as EditorTypes
import Inflex.Components.Cell.TextInput as TextInput
import Inflex.Frisson (View, casePosition)
import Inflex.Frisson as F
import Inflex.Schema as Shared
import Inflex.Types (OutputCell(..))
import Prelude
import Timed (timed)
import Web.HTML.Event.DragEvent as DE

--------------------------------------------------------------------------------
-- Component types

data Input = Input {
  cell :: OutputCell,
  cells :: Map UUID (OutputCell),
  dragger :: Dragger.Dragger
  }

data Query a
  = NestedCellError (View Shared.NestedCellError)

data Output
  = RemoveCell
  | UpdatePath (Maybe UUID) Shared.UpdatePath
  | RenameCell String
  | RepositionCell Int Int

data State = State
  { cell :: Cell
  , cells:: Map UUID (OutputCell)
  , dragger :: Dragger.Dragger
  , deleted :: Boolean
  }

data Command
  = SetCellFromInput Input
  | DeleteCell
  | TriggerUpdatePath (Maybe UUID) Shared.UpdatePath
  | TriggerRenameCell String
  | TriggerRepositionCell Int Int
  | DragElementAvailable (Manage.ElemRef Manage.Element)

-- derive instance genericCommand :: Generic Command _
-- instance showCommand :: Show Command where show x = genericShow x

derive instance genericInput :: Generic Input _
instance showInput :: Show Input where show x = genericShow x

--------------------------------------------------------------------------------
-- Internal types

data Cell = Cell
  { name :: String
  , code :: String
  , codeHash :: Shared.Hash
  , result :: Either (View Shared.CellError) (View Shared.Tree2)
  , resultHash :: Shared.Hash
  , type' :: Maybe (View Shared.TypeOf)
  , position :: View Shared.Position
  , uuid :: UUID
  }

derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where show x = genericShow x

instance eqCell :: Eq Cell where
  eq (Cell c1) (Cell c2) =
    c1.name == c2.name &&
    c1.codeHash == c2.codeHash &&
    c1.resultHash == c2.resultHash &&
    show c1.position == show c2.position

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\(Input {cell, cells, dragger}) ->
           State {cell: outputCellToCell cell, cells, dragger,deleted:false})
    , render: \state -> timed "Cell.render" (\_ -> render state)
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetCellFromInput
            , handleQuery = query
            }
    }

outputCellToCell :: OutputCell -> Cell
outputCellToCell (OutputCell cell) =
  Cell
    { name: cell.name
    , code: cell.code
    , codeHash: cell.codeHash
    , resultHash: cell.resultHash
    , result:
        F.caseResult
          { "ResultError": Left
          , "ResultOk": \resultTree -> Right (F.resultTreeTree resultTree)
          }
          (cell.result)
    , type': F.caseResult
        { "ResultError": \_ -> Nothing
        , "ResultOk": \resultTree -> Just (F.resultTreeTyp resultTree)
        }
        (cell.result)
    , position: cell.position
    , uuid: cell.uuid
    }

--------------------------------------------------------------------------------
-- Query

query ::
     forall a action output m t0 t1 x. Ord t1 => (MonadAff m)
  => Query a
  -> H.HalogenM State action (editor :: H.Slot EditorTypes.Query t0 t1 | x) output m (Maybe a)
query =
  case _ of
    NestedCellError cellError -> do
      log ("[Cell] Received error:" <> show cellError)
      _ <- H.queryAll (SProxy :: SProxy "editor")
                 (EditorTypes.NestedCellError cellError)
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

foreign import clearDragImage :: DE.DragEvent -> Effect Unit
foreign import setEmptyData :: DE.DragEvent -> Effect Unit

-- eval' :: forall q i m. MonadAff m =>  Command -> H.HalogenM State q i Output m Unit
-- eval' cmd = do
--   log (show cmd)
--   eval cmd


eval :: forall t184 t225 t227 t228.
  MonadAff t184 => Ord t227 => Command
                                  -> H.HalogenM State Command
                                       ( editor :: H.Slot EditorTypes.Query t228 t227
                                       | t225
                                       )
                                       Output
                                       t184
                                       Unit
eval =
  case _ of
    DragElementAvailable elemRef ->
      case elemRef of
        Manage.Created (element) ->
          case Manage.fromElement element of
            Just htmlelement -> do
              State {dragger} <- H.get
              void
                (H.subscribe
                   (Src.effectEventSource
                      (\emitter -> do
                         Dragger.attach
                           htmlelement
                           3
                           dragger
                           (\x y -> Src.emit emitter (TriggerRepositionCell x y))
                         pure mempty)))
            Nothing -> pure unit
        Manage.Removed _ -> pure unit
    TriggerUpdatePath muuid update -> do
      H.raise (UpdatePath muuid update)
    TriggerRenameCell update -> H.raise (RenameCell update)
    TriggerRepositionCell x y -> H.raise (RepositionCell x y)
    SetCellFromInput (Input {cell: c, cells, dragger}) -> do
      State {cell: oldCell, cells: oldCells} <- H.get
      let newCell@(Cell {name}) = outputCellToCell c
      if newCell /= oldCell
        then do
          log ("Cell.eval:SetCellFromInput: just updating.")
          H.modify_
            (\(State s) ->
               State (s {cell = newCell, cells = cells}))
        else do
          log ("Cell.eval:SetCellFromInput: running ResetDisplay")
          unless (cells == oldCells) $
             H.modify_ (\(State s) -> State (s {cells = cells}))
          _ <- H.queryAll (SProxy :: SProxy "editor") EditorTypes.ResetDisplay
          pure unit
    DeleteCell -> do
      H.modify_ (\(State s) -> State (s {deleted = true}))
      H.raise RemoveCell

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadAff m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot EditorTypes.Query EditorTypes.Output Unit,
                                             declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State { cell: Cell {name, code, result, type', position, uuid: UUID uuid}
              , cells
              , deleted
              })
  | deleted = HH.text ""
  | otherwise =
  HH.div
    [HP.class_ (HH.ClassName "cell-wrapper")
    ,HP.attr (AttrName "id") ("cell-" <> uuid)
    ,HP.attr (AttrName "style") (casePosition {
                                    "Unpositioned": "",
                                    "AbsolutePosition": \x y ->
                                       "position: absolute; left: " <> show x <> "px; top: " <> show y <> "px"
                                    } position)]
    [ HH.div
        [HP.class_ (HH.ClassName "cell")]
        [ HH.div
            [HP.class_ (HH.ClassName "cell-header")]
            [ HH.div [Manage.manage DragElementAvailable, HP.class_ (HH.ClassName "dragger"), HP.title "Drag the cell around with this"] [HH.text "✋"],
              HH.slot
                (SProxy :: SProxy "declname")
                unit
                (TextInput.component
                   (TextInput.Config
                      { placeholder: "Type a name here"
                      , unfilled: "(unnamed)"
                      , title: "Click to edit cell's name"
                      , validator: \_ -> true
                      }))
                (TextInput.Input
                   {text: name, notThese: mempty})
                (\name' -> pure (TriggerRenameCell name'))
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
                (EditorTypes.EditorAndCode
                   { editor: result
                   , code: code
                   , cells
                   , path: identity
                   , type': type'
                   })
                (\output ->
                   case output of
                     EditorTypes.UpdatePath muuid update -> Just (TriggerUpdatePath muuid update)
                     EditorTypes.NewCode text ->
                       Just
                         (TriggerUpdatePath
                            Nothing
                            (Shared.UpdatePath
                               { path: Shared.DataHere
                               , update:
                                   Shared.CodeUpdate (Shared.Code {text})
                               })))
            ]
        ]
    ]
