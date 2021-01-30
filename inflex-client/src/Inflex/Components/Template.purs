-- |

module Inflex.Components.Template where

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Prelude (Unit, pure, unit, (<<<))

--------------------------------------------------------------------------------
-- Interface

type Input = Unit

type Output = Unit

data Query a = SomeQuery

--------------------------------------------------------------------------------
-- Internal protocol

data Command
  = NoOp
  | HandleInput Input

data State =
  State

type Slots (i :: Type -> Type) = ()

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState:
        (\input ->
           State)
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
query _ = pure Nothing

--------------------------------------------------------------------------------
-- Eval

eval ::
     forall i t45 t48. MonadAff t45
  => Command
  -> H.HalogenM State t48 (Slots i) Output t45 Unit
eval cmd = pure unit

--------------------------------------------------------------------------------
-- Render

render ::
     forall a. MonadAff a
  => State
  -> HH.HTML (H.ComponentSlot HH.HTML (Slots Query) a Command) Command
render _ = HH.text "TODO: render!"
