-- |

module Inflex.Components.Cell.Editor.Types (
    EditorAndCode(..)
  , Output(..)
  , Query(..)
    , Input
  )
  where

import Inflex.Frisson (View, caseCellError, caseDataPath, caseFillError, caseMaybeRow, caseOriginalSource, caseTree2, caseTypeOf, caseVariantArgument, field2Key, field2Value, namedTypeName, namedTypeTyp, nestedCellErrorError, nestedCellErrorPath, rowFields)

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith, trim)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Manage as Manage
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Inflex.Components.Cell.TextInput as TextInput
import Inflex.Components.Code as Code
import Inflex.FieldName (validFieldName)
import Inflex.Schema (CellError)
import Inflex.Schema as Shared
import Inflex.Types (OutputCell)
import Prelude (class Eq, class Ord, class Show, Unit, bind, const, discard, map, max, mempty, min, pure, show, unit, (&&), (+), (-), (<<<), (<>), (==), (>), (>>=), negate, (/=))
import Web.DOM.Element (Element, fromEventTarget)
import Web.Event.Event (preventDefault, stopPropagation, currentTarget)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
import Web.UIEvent.MouseEvent (toEvent)
import Web.UIEvent.WheelEvent (toEvent) as Wheel

data EditorAndCode = EditorAndCode
  { editor :: Either (View Shared.CellError) (View Shared.Tree2)
  , code :: String
  , path :: Shared.DataPath -> Shared.DataPath
  , cells :: Map UUID (OutputCell)
  , type' :: Maybe (View Shared.TypeOf)
  }

instance editorAndCodeEq :: Eq EditorAndCode where
  eq (EditorAndCode x) (EditorAndCode y) =
    show (x.editor)==show (y.editor) && x.code==y.code && x.path Shared.DataHere == y.path Shared.DataHere

instance showEditorAndCode :: Show EditorAndCode where
  show (EditorAndCode e) = "EditorAndCode{code=" <> show (e.code) <> "}"

data Output
  = NewCode String
  | UpdatePath (Maybe UUID) Shared.UpdatePath

data Query a
 = NestedCellError (View Shared.NestedCellError)
 | ResetDisplay
 | NewInput Input

type Input = EditorAndCode
