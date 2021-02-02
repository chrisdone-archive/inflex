-- | A declaration in a document.

module Inflex.Components.Cell
  ( component
  , Input(..)
  , Query(..)
  , Cell(..)
  , Output(..)
  ) where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
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
import Web.HTML.Event.DragEvent as DE

--------------------------------------------------------------------------------
-- Component types

data Input = Input {
  cell :: Shared.OutputCell,
  namesInScope :: Array String
  }

data Query a
  = NestedCellError Shared.NestedCellError

data Output
  = RemoveCell
  | UpdatePath Shared.UpdatePath
  | RenameCell String

data State = State
  { cell :: Cell
  , namesInScope:: Array String
  }

data Command
  = SetCellFromInput Input
  | DeleteCell
  | TriggerUpdatePath Shared.UpdatePath
  | TriggerRenameCell String

derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show x = genericShow x

derive instance genericInput :: Generic Input _
instance showInput :: Show Input where show x = genericShow x

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

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\(Input {cell, namesInScope}) ->
           State {cell: outputCellToCell cell, namesInScope})
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< SetCellFromInput
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
     forall a action output m t0 t1 x. Ord t1 => (MonadAff m)
  => Query a
  -> H.HalogenM State action (editor :: H.Slot Editor.Query t0 t1 | x) output m (Maybe a)
query =
  case _ of
    NestedCellError cellError -> do
      log ("[Cell] Received error:" <> show cellError)
      _ <- H.queryAll (SProxy :: SProxy "editor")
                 (Editor.NestedCellError cellError)
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

foreign import clearDragImage :: DE.DragEvent -> Effect Unit
foreign import setEmptyData :: DE.DragEvent -> Effect Unit

-- eval' :: forall q i m. MonadAff m =>  Command -> H.HalogenM State q i Output m Unit
-- eval' cmd = do
--   log (show cmd)
--   eval cmd

eval :: forall q i m. MonadAff m =>  Command -> H.HalogenM State q i Output m Unit
eval =
  case _ of
    TriggerUpdatePath update -> H.raise (UpdatePath update)
    TriggerRenameCell update -> H.raise (RenameCell update)
    SetCellFromInput (Input {cell: c, namesInScope}) -> do
      let cell@(Cell {hash, name}) = outputCellToCell c
      H.modify_
        (\(State s) -> State (s {cell = cell, namesInScope = namesInScope}))
    DeleteCell -> H.raise RemoveCell

--------------------------------------------------------------------------------
-- Render

render :: forall keys q m. MonadAff m =>
          State
       -> HH.HTML (H.ComponentSlot HH.HTML ( editor :: H.Slot Editor.Query Editor.Output Unit,
                                             declname :: H.Slot q String Unit | keys) m Command)
                  Command
render (State { cell: Cell {name, code, result, hash}
              , namesInScope
              }) =
  HH.div
    [HP.class_ (HH.ClassName "cell-wrapper")]
    [ HH.div
        [HP.class_ (HH.ClassName "cell")]
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
                (Editor.EditorAndCode
                   { editor: either Editor.ErrorE identity result
                   , code: code
                   , namesInScope
                   , path: identity
                   })
                (\output ->
                   case output of
                     Editor.UpdatePath update -> Just (TriggerUpdatePath update)
                     Editor.NewCode text ->
                       Just
                         (TriggerUpdatePath
                            (Shared.UpdatePath
                               { path: Shared.DataHere
                               , update:
                                   Shared.CodeUpdate (Shared.Code {text})
                               })))
            ]
        ]
    ]
