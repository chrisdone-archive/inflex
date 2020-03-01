module Inflex.Dec
  ( component
  , Dec(..)
  ) where


import Data.Either
import Data.Foldable
import Data.Map (Map)
import Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple
import Effect.Class
import Effect.Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Web.HTML.HTMLElement as Web
import Web.UIEvent.KeyboardEvent as K

data Dec = Dec {
    name :: String
  , rhs :: String
  , result :: Either String String

  }

data State = State {
    dec :: Dec
  , display :: Display
  }

data Display
  = DisplayResult
  | DisplayEditor String
  | DisplayTable (Set String) (Array (Map String String))

data Command = StartEditor | SetInput String | FinishEditing String | SetDec Dec | Autoresize

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Dec Dec m
component =
  H.mkComponent
    { initialState: (\dec -> State {dec, display: DisplayResult})
    , render
    , eval: H.mkEval H.defaultEval { handleAction = eval, receive = pure <<< SetDec }
    }

render (State {dec: Dec {name, rhs, result}, display}) =
  HH.div
    [HP.class_ (HH.ClassName "dec")]
    [ HH.span [HP.class_ (HH.ClassName "dec-name")] [HH.text name]
    , HH.span [HP.class_ (HH.ClassName "dec-eq")] [HH.text "="]
    , HH.span
        [HP.class_ (HH.ClassName "dec-rhs")]
        [ case display of
            DisplayEditor string ->
              HH.div [] [HH.input
                           [ HP.value string
                           , HP.class_ (HH.ClassName "editor")
                           , HP.ref editorRef
                           , HE.onKeyUp
                               (\k ->
                                  case K.code k of
                                    "Enter" -> Just (FinishEditing string)
                                    code -> Just (Autoresize))
                           , HE.onValueChange (\i -> pure (SetInput i))
                           ]
                        ]
            DisplayResult ->
              HH.span
                [HE.onClick (\_ -> pure StartEditor)]
                [HH.text (either identity identity result)]
            DisplayTable heading rows ->
               HH.table [] [HH.thead [] (map (\text -> HH.th [] [HH.text text]) (Set.toUnfoldable heading))
                           ,HH.tbody []
                                     (map (\row -> HH.tr []
                                                         (map (\(Tuple k v) -> HH.td [] [HH.text v])
                                                              (M.toUnfoldable row)))
                                          rows)]
        ]
    ]

eval =
  case _ of
    StartEditor -> do
      H.modify_
        (\(State st) ->
           State
             (st
                { display =
                    DisplayEditor
                      (let Dec {rhs} = st . dec
                        in rhs)
                }))
    FinishEditing rhs -> do
      H.liftEffect (log ("Finish editing with rhs=" <> rhs))
      State st <- H.get
      let newDec =
            let Dec dec = st . dec
             in Dec (dec {rhs = rhs, result = Left "Waiting..."})
      H.modify_
        (\(State st') -> State (st' {display = DisplayResult, dec = newDec}))
      _result <- H.raise newDec
      H.modify_ (\(State st') -> State (st' {display = DisplayResult}))
    SetInput i ->
      H.modify_ (\(State st) -> State (st {display = DisplayEditor i}))
    SetDec dec -> H.put (State {dec, display: DisplayResult})
    Autoresize -> do
      ref <- H.getHTMLElementRef editorRef
      -- TODO: Set style="width: 100ch"  where 100=length of input.
      H.liftEffect (for_ ref (\el -> pure unit))
    {-NewTable ->
     H.modify_ (\(State s) ->
       State (s { display = DisplayTable (Set.fromFoldable ["Name","Age"])
                                         [M.fromFoldable [Tuple "Name" "Chris", Tuple "Age" "31"]
                                         ,M.fromFoldable [Tuple "Name" "Giulia", Tuple "Age" "30"]]
                }
             )
       )-}

editorRef :: H.RefLabel
editorRef = (H.RefLabel "editor")
