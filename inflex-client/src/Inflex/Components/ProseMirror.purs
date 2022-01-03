-- | ProseMirror component.

module Inflex.Components.ProseMirror
  ( component
  , Input, Query, Config, Output
  ) where

import Data.Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Halogen (Component, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, liftEffect, mkComponent, mkEval, put, raise, subscribe) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (effectEventSource, emit) as H
import Prelude (class Show, Unit, bind, const, discard, mempty, pure, unit, void, (/=), (<<<))
import Web.HTML.HTMLElement (HTMLElement)

--------------------------------------------------------------------------------
-- Interface

type Input = Unit

type Output = Unit

data Query a = NoOp

--------------------------------------------------------------------------------
-- Internal protocol

type Slots a = ()

data State = State
  {
  }

data Command
  = Initializer

--------------------------------------------------------------------------------
-- Types

data Config = Config
  {
  }

foreign import data ProseMirror :: Type

data PMEvent
  = NoEvents

derive instance genericPMEvent :: Generic PMEvent _
instance showPMEvent :: Show PMEvent where show x = genericShow x

--------------------------------------------------------------------------------
-- Constants

refLabel :: H.RefLabel
refLabel = H.RefLabel "prosemirror"

--------------------------------------------------------------------------------
-- Component

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval
              H.defaultEval
                { handleAction = eval
                -- , receive = pure <<< SetConfig
                , initialize = Just Initializer
                , handleQuery = query
                }
    }
  where
    initialState :: Input -> State
    initialState (config) = State {}

--------------------------------------------------------------------------------
-- Query

query ::
     forall i m a. (MonadAff m)
  => Query a
  -> H.HalogenM State Command (Slots i) Output m (Maybe a)
query =
  case _ of
    NoOp -> pure Nothing

--------------------------------------------------------------------------------
-- Eval

eval :: forall i t9. MonadEffect t9 => MonadAff t9 => Command -> H.HalogenM State Command (Slots i) Output t9 Unit
-- eval (CMEventIn event) =
--   H.raise (CMEventOut event)
eval Initializer = do
  State {} <- H.get
  melement <- H.getHTMLElementRef refLabel
  case melement of
    Nothing -> pure unit
    Just element -> do
      cm <- H.liftEffect (proseMirror element)
      void
        (H.subscribe
           (H.effectEventSource
              (\emitter -> do
                 pure mempty)))

--------------------------------------------------------------------------------
-- Render

render :: forall t1 t3 t4. t1 -> HH.HTML t4 t3
render = const (HH.div [HP.ref refLabel] [])

--------------------------------------------------------------------------------
-- Foreign

foreign import proseMirror
  :: HTMLElement
  -> Effect ProseMirror
