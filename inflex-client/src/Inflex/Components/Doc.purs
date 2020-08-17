-- |

module Inflex.Components.Doc
  ( component
  ) where

import Control.Monad.State (class MonadState)
import Data.Map (Map)
import Data.UUID (UUID)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Inflex.Components.Cell as Cell
import Prelude

--------------------------------------------------------------------------------
-- Types

data Command
  = Initialize
  | UpdateCell UUID
              Cell.Cell
  | NewCell
  | DeleteCell UUID

type State = {
  decs :: Map UUID Cell.Cell
 }

type Input = Unit

type Output = Unit

--------------------------------------------------------------------------------
-- Component

component :: forall q. H.Component HH.HTML q Input Output Aff
component =
  H.mkComponent
    { initialState: const {decs: mempty}
    , render
    , eval:
        H.mkEval
          H.defaultEval {initialize = pure Initialize, handleAction = eval}
    }

--------------------------------------------------------------------------------
-- Render

render
  :: forall q keys m. MonadEffect m
  => State
  -> HH.HTML (H.ComponentSlot HH.HTML ( "Cell" :: H.Slot q Cell.Output String | keys) m Command) Command
render state = HH.div [] [HH.text "OK, go!"]

--------------------------------------------------------------------------------
-- Eval

eval ::
     forall m. MonadState State m
  => MonadEffect m
  => MonadAff m
  => Command
  -> m Unit
eval =
  case _ of
    Initialize -> pure unit
    NewCell -> pure unit
    DeleteCell uuid -> pure unit
    UpdateCell uuid dec -> pure unit
