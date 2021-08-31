-- |

module Inflex.Components.Code
  ( component
  , Input(..)
  , Output(..)
  , Query(..)
  , Command(..)
  ) where

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, uuidToString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (error)
import Halogen as H
import Halogen.HTML as HH
import Inflex.Components.CodeMirror as CM
import Inflex.Lexer (Token(..), lexString)
import Inflex.Types (OutputCell(..))
import Prelude (Unit, bind, discard, map, pure, unit, ($), (-), (<<<), (<>))

--------------------------------------------------------------------------------
-- Foreign

foreign import meta :: {
  prims :: Array { name :: String, display :: String }
 }

prims :: Map String String
prims = M.fromFoldable (map (\prim -> Tuple (prim.name) (prim.display)) (meta.prims))

--------------------------------------------------------------------------------
-- Interface

data Input = Input
  { code :: String
  , cells :: Map UUID (OutputCell)
  }

data Output =
  TextOutput String

data Query a = SetCells (Map UUID (OutputCell))

--------------------------------------------------------------------------------
-- Internal protocol

data Command
  = HandleInput Input
  | CMEvent CM.CMEvent

data State = State
  { code :: String
  , cells :: Map UUID (OutputCell)
  }

type Slots (i :: Type -> Type) =
  ( codemirror :: H.Slot CM.Query CM.Output Unit
  )

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\(Input input) ->
           State
             { code: input . code
             , cells: input . cells
             })
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = eval
            , receive = pure <<< HandleInput
            , handleQuery = query
            }
    }

--------------------------------------------------------------------------------
-- Query

query ::
     forall i m a. (MonadAff m)
  => Query a
  -> H.HalogenM State Command (Slots i) Output m (Maybe a)
query =
  case _ of
    SetCells cells -> do
      H.modify_ (\(State s) -> State (s {cells = cells}))
      pure Nothing

--------------------------------------------------------------------------------
-- Eval

eval ::
     forall i t45 t48. MonadAff t45
  => Command
  -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval =
  case _ of
    HandleInput (Input {cells}) ->
      H.modify_ (\(State s) -> State (s {cells = cells}))
    CMEvent event -> do
      case event of
        CM.Entered -> do
          mvalue <-
            H.query
              (SProxy :: SProxy "codemirror")
              unit
              (H.request CM.GetTextValue)
          case mvalue of
            Just value -> do
              H.raise (TextOutput value)
            Nothing -> pure unit
        CM.Picked uuid -> do
          mvalue <-
            H.query
              (SProxy :: SProxy "codemirror")
              unit
              (H.request CM.GetTextValue)
          case mvalue of
            Just value -> do
              State state <- H.get
              setMarks <- H.liftEffect $ makeSetMarks (state . cells) value
              traverse_ (H.query (SProxy :: SProxy "codemirror") unit) setMarks
            Nothing -> pure unit
        _ -> pure unit

--------------------------------------------------------------------------------
-- Render

render ::
     forall a. MonadAff a
  => State
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
render (State state) =
  HH.slot
    (SProxy :: SProxy "codemirror")
    unit
    CM.component
    (CM.Config
       { internalConfig:
       { readOnly: false
       , theme: "default"
       , selection: CM.noSelection
       , mode: "haskell"
       , value: state.code
       , styleActiveLine: true
       , lineNumbers: false
       , lineWrapping: true
       , autofocus: true
       , autoCloseBrackets: true
       , highlightSelectionMatches: true
       , namesInScope:
          map (\(Tuple _key (OutputCell cell)) ->
                 let uuid = cell.uuid
                     name = cell.name
                 in
                -- let  (Shared.OutputCell{uuid: UUID uuid, name})
                 { text: "@uuid:" <> uuidToString uuid, -- what will be inserted
                   key: uuidToString uuid, -- what will be raised later to PS
                   displayText: name, -- we can put whatever in here, and even a render function
                   -- <https://codemirror.net/doc/manual.html#addons>
                   matchText: name -- string that will match this identifier
                 })
              (M.toUnfoldable (state.cells)) <>
          map (\prim ->
                 { text: "@prim:" <> prim.name, -- what will be inserted
                   key: prim.name, -- what will be raised later to PS
                   displayText: prim.display, -- we can put whatever in here, and even a render function
                   -- <https://codemirror.net/doc/manual.html#addons>
                   matchText: prim.display -- string that will match this identifier
                 })
              (meta.prims)
       }
       , initializers: makeSetMarks (state.cells) (state.code) })
    (case _ of
       CM.CMEventOut event -> Just (CMEvent event))

makeSetMarks :: forall t55. Map UUID (OutputCell) -> String -> Effect (Array (CM.Query t55))
makeSetMarks cells code = do
          result <- lexString (code)
          case result of
            Left _ -> do error ("Lexing failed! " <> code)
                         pure []
            Right tokens -> do
              -- log (show tokens)
              pure (Array.mapMaybe
                (\token ->
                   case token of
                     MiscToken -> Nothing
                     PrimToken key location ->
                       case M.lookup key prims of
                          Nothing -> Nothing
                          Just display ->
                            Just (CM.MarkText
                                  { line: location . start . line - 1
                                  , ch: location . start . column - 1
                                  }
                                  { line: location . end . line - 1
                                  , ch: location . end . column - 1
                                  }
                                  {
                                    replaceText: display
                                  })
                     UuidToken uuid location ->
                        case M.lookup uuid (cells) of
                          Nothing -> Nothing
                          Just cell ->
                            Just (CM.MarkText
                                  { line: location . start . line - 1
                                  , ch: location . start . column - 1
                                  }
                                  { line: location . end . line - 1
                                  , ch: location . end . column - 1
                                  }
                                  {
                                    replaceText: let OutputCell{name}= cell in name
                                  }))
                tokens)
