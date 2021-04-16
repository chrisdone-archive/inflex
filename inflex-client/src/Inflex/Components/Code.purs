-- |

module Inflex.Components.Code
  ( component
  , Input(..)
  , Output(..)
  , Query(..)
  , Command(..)
  ) where

import Data.Array as Array
import Data.Either
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console
import Halogen as H
import Halogen.HTML as HH
import Inflex.Components.CodeMirror as CM
import Inflex.Lexer
import Inflex.Schema as Shared
import Prelude
import Prelude (Unit, bind, discard, map, pure, unit, void, when, ($), (&&), (-), (<<<), (<>), (==))

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
  , cells :: Map UUID Shared.OutputCell
  }

data Output =
  TextOutput String

data Query a = SetCells (Map UUID Shared.OutputCell)

--------------------------------------------------------------------------------
-- Internal protocol

data Command
  = HandleInput Input
  | CMEvent CM.CMEvent

data State = State
  { code :: String
  , cells :: Map UUID Shared.OutputCell
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
    HandleInput _ -> pure unit
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
              pure unit
              -- traverse_
              --   (\token ->
              --    case M.lookup uuid (state.namesInScope) of
              --      Nothing -> pure unit
              --      Just displayText ->
              --        when(token.tag == "uuid" && token.text == uuid) $
              --        void $ H.query(SProxy :: SProxy "codemirror") unit
              --          (CM.MarkText
              --             { line: token . location . start . line - 1
              --             , ch: token . location . start . column - 1
              --             }
              --             { line: token . location . end . line - 1
              --             , ch: token . location . end . column - 1
              --             }
              --             {
              --               replaceText: displayText
              --             }
              --          ))
              --   (lexer value)
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
       , namesInScope: []
          -- map(\(Tuple uuid v) ->
          --                   { text: "@uuid:" <> uuid, -- what will be inserted
          --                     key: uuid, -- what will be raised later to PS
          --                     displayText: v, -- we can put whatever in here, and even a render function
          --                     -- <https://codemirror.net/doc/manual.html#addons>
          --                     matchText: v -- string that will match this identifier
          --                   } )
          --            (M.toUnfoldable (state.namesInScope))
       }
       , initializers: initializers})
    (case _ of
       CM.CMEventOut event -> Just (CMEvent event))
  where initializers = do
          result <- lexString (state.code)
          case result of
            Left _ -> do error ("Lexing failed! " <> state.code)
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
                        case M.lookup uuid (state.cells) of
                          Nothing -> Nothing
                          Just (Shared.OutputCell{name}) ->
                            Just (CM.MarkText
                                  { line: location . start . line - 1
                                  , ch: location . start . column - 1
                                  }
                                  { line: location . end . line - 1
                                  , ch: location . end . column - 1
                                  }
                                  {
                                    replaceText: name
                                  }))
                tokens)
