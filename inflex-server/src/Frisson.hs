{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Frisson where

import qualified Data.Aeson as Aeson
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified Language.Haskell.TH as TH

--------------------------------------------------------------------------------
-- Types

data Rep
  = ProductRep Product
  | SumRep (NonEmpty Cons)
  | RecordRep Record
  | EnumRep (NonEmpty ConsName)
  | NewtypeRep Newtype
  | SingletonRep ConsName
  deriving (Show)

data Record = Record
  { name :: ConsName
  , fields :: NonEmpty Field
  } deriving (Show)

data Product = Product
  { name :: ConsName
  , slots :: (NonEmpty Type)
  } deriving (Show)

data Newtype = Newtype
  { name :: ConsName
  , mfield :: Maybe FieldName
  , slot :: Type
  } deriving (Show)

data Field = Field
  { name :: FieldName
  , typ :: Type
  } deriving (Show)

data Cons = Cons
  { name :: ConsName
  , slots :: [Type]
  } deriving (Show)

data Type
  = ConType TypeName
  | IntType
  | TextType
  | ArrayType Type
  deriving (Show)

newtype ConsName =
  ConsName TH.Name
  deriving (Show)

newtype FieldName =
  FieldName TH.Name
  deriving (Show)

newtype TypeName =
  TypeName TH.Name
  deriving (Show)

--------------------------------------------------------------------------------
-- Top-level entry point

derive :: TH.Name -> TH.Q [TH.Dec]
derive n = do
  i <- TH.reify n
  let rep = resolveInfo i
  TH.reportWarning (show (foreignsFromNameRep (TypeName n) rep))
  generateToJSONInstance n rep

--------------------------------------------------------------------------------
-- Resolving Haskell types to Rep

resolveInfo :: TH.Info -> Rep
resolveInfo (TH.TyConI dec) = resolveDec dec
resolveInfo _ = error "Must be a data type."

resolveDec :: TH.Dec -> Rep
resolveDec (TH.NewtypeD _ctx _name _tys _mkind (TH.NormalC name [bangType]) _deriv) =
  NewtypeRep
    Newtype
      { name = ConsName name
      , slot = resolveType (snd bangType)
      , mfield = Nothing
      }
resolveDec (TH.NewtypeD _ctx _name _tys _mkind (TH.RecC name [(field,_bang,typ)]) _deriv) =
  NewtypeRep
    Newtype
      { name = ConsName name
      , slot = resolveType typ
      , mfield = pure (FieldName field)
      }
resolveDec (TH.DataD _ctx _name _tys _mkind cons _deriv) =
  case cons of
    [] -> error "No constructors."
    [TH.NormalC name bangTypes] ->
      case NE.nonEmpty bangTypes of
        Just ((_, typ) :| []) ->
          NewtypeRep
            Newtype
              {name = (ConsName name), slot = resolveType typ, mfield = Nothing}
        Just types ->
          ProductRep
            Product
              {name = ConsName name, slots = fmap (resolveType . snd) types}
        Nothing -> SingletonRep (ConsName name)
    [TH.RecC name bangTypes] ->
      case NE.nonEmpty bangTypes of
        Just ((fname, _bang, typ) :| []) ->
          NewtypeRep
            Newtype
              {name = (ConsName name), slot = resolveType typ, mfield = pure (FieldName fname)}
        Just types ->
          RecordRep
            Record
              { name = ConsName name
              , fields =
                  fmap
                    (\(name', _bang, typ) ->
                       Field {typ = resolveType typ, name = FieldName name'})
                    types
              }
        Nothing -> error "A record type MUST have fields."
    conses
      | Just necons <- traverse maybeConsNameOnly conses >>= NE.nonEmpty ->
        EnumRep necons
      | Just neconses <- NE.nonEmpty (map resolveCons conses) -> SumRep neconses
    _ -> error "Invalid type to resolve."
resolveDec _ = error "Must be a data type."

resolveCons :: TH.Con -> Cons
resolveCons (TH.NormalC name bangTypes) =
  Cons {name = ConsName name, slots = fmap (resolveType . snd) bangTypes}
resolveCons _ = error "Invalid Con type."

maybeConsNameOnly :: TH.Con -> Maybe ConsName
maybeConsNameOnly (TH.NormalC name []) = pure (ConsName name)
maybeConsNameOnly _ = Nothing

resolveType :: TH.Type -> Type
resolveType =
  \case
    TH.ConT name
      | name == ''Int -> IntType
      | name == ''Text -> TextType
      | otherwise -> ConType (TypeName name)
    TH.AppT (TH.ConT con) name
      | con == ''Vector -> ArrayType (resolveType name)
    _ -> error "Unsupported type."

--------------------------------------------------------------------------------
-- Generate Haskell-side

-- TODO: Generate FromJSON/ToJSON instances using a Rep

generateToJSONInstance :: TH.Name -> Rep -> TH.Q [TH.Dec]
generateToJSONInstance name rep = do
  func <- generateToJSONMethod rep
  [d| instance Aeson.ToJSON $(TH.conT name) where toJSON = $(pure func) |]

generateToJSONMethod :: Rep -> TH.Q TH.Exp
generateToJSONMethod =
  \case
    SingletonRep {} -> TH.lamE [TH.wildP] (TH.conE 'Aeson.Null)
    ProductRep Product {name = ConsName cons, slots} ->
      let (pat, rhs) = productPatAndRhs cons slots []
       in TH.lamE [pat] rhs
    NewtypeRep Newtype {name = ConsName cons} ->
      TH.lamE [TH.conP cons [varP 0]] (TH.appE (TH.varE 'Aeson.toJSON) (varE 0))
    RecordRep Record {name, fields} ->
      generateToJSONMethod
        (ProductRep Product {name, slots = (fmap (\Field {typ} -> typ) fields)})
    SumRep conses ->
      TH.lamCaseE
        [ TH.match pat (TH.normalB rhs) []
        | (i, Cons {name = ConsName name, slots}) <- zip [0 ..] (toList conses)
        , let (pat, rhs) = productPatAndRhs name slots [toJSON (int i)]
        ]
    EnumRep conses ->
      TH.lamCaseE
        [ TH.match pat (TH.normalB rhs) []
        | (i, ConsName name) <- zip [0 ..] (toList conses)
        , let pat = TH.conP name []
              rhs = TH.appE (TH.varE 'Aeson.toJSON) (int i)
        ]
  where
    varP = TH.varP . var
    varE = TH.varE . var
    int i = TH.sigE (TH.litE (TH.integerL i)) (TH.conT ''Int)
    var (i :: Int) = TH.mkName ("v_" ++ show i)
    productPatAndRhs cons slots prefix =
      (,)
        (TH.conP cons (zipWith (const . varP) [0 ..] (toList slots)))
        (toJSON
           (TH.listE
              (prefix ++ zipWith (const . toJSON . varE) [0 ..] (toList slots))))
    toJSON = TH.appE (TH.varE 'Aeson.toJSON)

--------------------------------------------------------------------------------
-- Generate PureScript-side

data ForeignImport = ForeignImport
  { name :: TypeName
  , suffix :: Suffix
  , signature :: Signature
  } deriving (Show)

data Suffix
  = FieldSuffix FieldName Int -- ^ Access the field at index i.
  | SlotSuffix Int -- ^ Access the slot at index i.
  | IdFieldSuffix FieldName -- ^ The action is a no-op; identity.
  | IdSuffix -- ^ The action is a no-op; identity.
  | ViewSuffix -- ^ Simply a type-changing op from Json to View T.
  | UnviewSuffix -- ^ Reverse of ViewSuffix.
  deriving (Show)

data Signature
  = ViewToThing TypeName Type
  | ThingToView TypeName Type
  | JsonView TypeName
  | JsonUnview TypeName
  deriving (Show)

-- | Produce a foreign import which can be used to generate both a
-- "foreign import .." statement, and also the related JavaScript.
foreignsFromNameRep :: TypeName -> Rep -> Seq ForeignImport
foreignsFromNameRep name =
  \case
    SingletonRep {} -> mempty
    RecordRep Record {fields} ->
      mconcat [viewUnview name, foreignForFields name fields]
    ProductRep Product {slots} ->
      mconcat [viewUnview name, foreignForSlots name slots]
    NewtypeRep Newtype {mfield, slot} ->
      mconcat
        [ viewUnview name
        , pure (foreignForNewtype name mfield slot)
        ]
    _ -> mempty

-- | Generate foreign impor for a newtype. In any case, it's identity,
-- a no-op, but depending on whether there's a field, will generate a
-- different name for the foreign import.
foreignForNewtype :: TypeName -> Maybe FieldName -> Type -> ForeignImport
foreignForNewtype typeName mfield fieldType =
  ForeignImport
    { name = typeName
    , suffix = maybe IdSuffix IdFieldSuffix mfield
    , signature = ViewToThing typeName fieldType
    }

-- | Generate the view/unview foreign imports.
--
-- View/unview. O(1) operation; no-op:
--
-- foreign import docView :: Json -> View Doc
-- foreign import docUnview :: View Doc -> Json
--
viewUnview :: TypeName -> Seq ForeignImport
viewUnview name =
  Seq.fromList
    [ ForeignImport
        {name = name, suffix = ViewSuffix, signature = JsonView name}
    , ForeignImport
        {name = name, suffix = UnviewSuffix, signature = JsonUnview name}
    ]

-- | Generate product slot accessors:
--
-- foreign import docSlot1 :: View Doc -> Array (View Cell)
-- foreign import cellSlot2 :: View Cell -> String
--
foreignForSlots :: Foldable t => TypeName -> t Type -> Seq ForeignImport
foreignForSlots name slots =
  Seq.fromList
    [ ForeignImport
      { name = name
      , suffix = SlotSuffix i
      , signature = ViewToThing name fieldType
      }
    | (i, fieldType) <- zip [0..] (toList slots)
    ]

-- | Generate record field accessors:
--
-- foreign import docCells :: View Doc -> Array (View Cell)
-- foreign import cellUUID :: View Cell -> String
--
foreignForFields :: Foldable t => TypeName -> t Field -> Seq ForeignImport
foreignForFields name fields =
  Seq.fromList
    [ ForeignImport
      { name = name
      , suffix = FieldSuffix fieldName i
      , signature = ViewToThing name fieldType
      }
    | (i, Field {name = fieldName, typ = fieldType}) <- zip [0..] (toList fields)
    ]

-- | Generate foreign import for a sum type:
--
-- Example:
--
-- data CsvColumnType
--   = IntegerType Optionality
--   | DecimalType Int Optionality
--   | TextType Optionality
--
-- foreign import caseCsvColumnType
--   :: forall r.
--      View CsvColumnType
--      -> {
--            "IntegerType": View Optionality -> r,
--            "DecimalType": Int -> View Optionality -> r,
--            "TextType": View Optionality -> r
--         }
--      -> r
--
-- Constructors without any slots are just @"Foo": r@.
--
foreignForSum :: ForeignImport
foreignForSum = undefined
