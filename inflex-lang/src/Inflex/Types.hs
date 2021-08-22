{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Project-wide shared types.

module Inflex.Types where

import           Control.DeepSeq
import           Data.Hashable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import           Data.String
import           Data.Text (Text)
import           Data.UUID (UUID)
import           Data.Vector (Vector)
import           Data.Void
import           FlatParse.Basic as FlatParse
import           GHC.Generics
import           Inflex.Types.SHA512
import           Language.Haskell.TH.Lift
import           Numeric.Natural

--------------------------------------------------------------------------------
-- AST types

data Expression s where
  LiteralExpression :: !(Literal s) -> Expression s
  LambdaExpression :: !(Lambda s) -> Expression s
  ApplyExpression :: !(Apply s) -> Expression s
  VariableExpression :: !(Variable s) -> Expression s
  GlobalExpression :: !(Global s) -> Expression s
  LetExpression :: !(Let s) -> Expression s
  InfixExpression :: !(Infix s) -> Expression s
  RecordExpression :: !(Record s) -> Expression s
  PropExpression :: !(Prop s) -> Expression s
  ArrayExpression :: !(Array s) -> Expression s
  HoleExpression :: !(Hole s) -> Expression s
  VariantExpression :: !(Variant s) -> Expression s
  IfExpression :: !(If s) -> Expression s
  CaseExpression :: !(Case s) -> Expression s
  EarlyExpression :: !(Early s) -> Expression s
  BoundaryExpression :: !(Boundary s) -> Expression s

data Boundary s = Boundary
  { location :: !(StagedLocation s)
  , typ :: !(StagedType s)
  , expression :: Expression s
  }

data Early s = Early
  { location :: !(StagedLocation s)
  , typ :: !(StagedType s)
  , expression :: Expression s
  }

data Case s = Case
  { location :: !(StagedLocation s)
  , typ :: !(StagedType s)
  , scrutinee :: !(Expression s)
  , alternatives :: !(NonEmpty (Alternative s))
  }

data Alternative s = Alternative
  { location :: !(StagedLocation s)
  , pattern' :: !(Pattern s)
  , expression :: !(Expression s)
  }

data Pattern s
  = ParamPattern (Param s)
  | VariantPattern (VariantP s)

data VariantP s = VariantP
  { location :: !(StagedLocation s)
  , tag :: !TagName
  , argument :: !(Maybe (Param s))
  }

data If s = If
  { location :: !(StagedLocation s)
  , typ :: !(StagedType s)
  , condition :: !(Expression s)
  , consequent :: !(Expression s)
  , alternative :: !(Expression s)
  }

data Variant s = Variant
  { location :: !(StagedLocation s)
  , typ :: !(StagedType s)
  , tag :: !TagName
  , argument :: !(Maybe (Expression s))
  }

data Hole s = Hole
  { location :: !(StagedLocation s)
  , typ :: !(StagedType s)
  }

data TermForm
  = Evaluated
  | Unevaluated

data Array s = Array
  { expressions :: !(Vector (Expression s))
  , typ :: !(StagedType s)
  , location :: !(StagedLocation s)
  , form :: !(StagedForm s)
  }

data Prop s = Prop
  { expression :: !(Expression s)
  , name :: !FieldName
  , typ :: !(StagedType s)
  , location :: !(StagedLocation s)
  }

data Record s = Record
  { fields :: ![FieldE s]
  , location :: !(StagedLocation s)
  , typ :: !(StagedType s)
  }

data FieldE s = FieldE
  { name :: !FieldName
  , expression :: !(Expression s)
  , location :: !(StagedLocation s)
  }

data Infix s = Infix
  { location :: !(StagedLocation s)
  , global :: !(StagedOp s)
  , left :: !(Expression s)
  , right :: !(Expression s)
  , typ :: !(StagedType s)
  }

data Let s = Let
  { location :: !(StagedLocation s)
  , binds :: !(NonEmpty (Bind s))
  , body :: !(Expression s)
  , typ :: !(StagedType s)
  }

-- | A thing named in the document.
data Named a = Named
  { uuid :: Uuid
  , name :: Text
  , order :: Int
  , code :: Text
  , thing :: a
  , sourceHash :: !SourceHash
  , dependencies :: Set Uuid
  } deriving (Show, Lift, Eq, Ord, Functor, Generic, Traversable, Foldable)
instance NFData a => NFData (Named a)

data SourceHash
  = HashNotKnownYet
  | HashKnown SHA512
  deriving (Show, Lift, Eq, Ord, Generic)
instance NFData SourceHash

newtype Uuid = Uuid Text
 deriving (Eq, Ord, Show, Lift, NFData, Hashable)

-- | A ref @foo in the source code.
data Ref
  = UuidRef !UUID
  | Sha512Ref !SHA512
  deriving (Show, Eq, Ord)

-- | A "Cell" is a binding that is going to be evaluated and displayed
-- in the document. Cells are polymorphic and have type-class
-- constraints, some of which may have been defaulted.
data Cell = Cell
  { location :: !Cursor
  , defaulted :: !(Expression Resolved)
  , scheme :: !(Scheme Polymorphic)
  , defaultedClassConstraints :: !(Seq (Default Polymorphic))
  , ambiguousClassConstraints :: !(Seq (ClassConstraint Polymorphic))
  }

-- | A "Cell" is a binding that is going to be evaluated and displayed
-- in the document. Cells are polymorphic and have type-class
-- constraints, some of which may have been defaulted.
data Cell1 = Cell1
  { location :: !Cursor
  , defaulted :: !(Expression Resolved)
  , parsed :: !(Expression Parsed)
  , scheme :: !(Scheme Polymorphic)
  , mappings :: !(Map Cursor SourceLocation)
  , defaultedClassConstraints :: !(Seq (Default Polymorphic))
  , ambiguousClassConstraints :: !(Seq (ClassConstraint Polymorphic))
  , sourceHash :: !SHA512
  }

-- | A defaulted class constraint. Contains the original and defaulted
-- version, and the instance produced.
data Default s = Default
  { classConstraintDefaulted :: !(ClassConstraint s)
  , classConstraintOriginal :: !(ClassConstraint s)
  , instanceName :: !InstanceName
  }

data Bind s = Bind
  { location :: !(StagedLocation s)
  , param :: !(Param s)
  , value :: !(Expression s)
  , typ :: !(StagedType s)
  }

data Global s = Global
  { location :: !(StagedLocation s)
  , name :: !(StagedGlobalName s)
  , scheme :: !(StagedScheme s)
  }

data ParsedGlobal
  = ParsedHash Hash
  | ParsedTextName Text
  | ParsedUuid Uuid
  | ParsedPrim Function
  -- The ParsedFromInteger/ParsedFromDecimal cases below are hacks to
  -- avoid doing more work. I'd prefer something cleaner,
  -- perhaps. Although I'm not sure what. Perhaps dropping or
  -- collapsing the whole ParsedGlobal type? GlobalRef already has
  -- many of these constructors.
  | ParsedFromInteger
  | ParsedFromDecimal

data IncompleteGlobalRef
  = ExactGlobalRef (GlobalRef Renamed)
  | ResolvedGlobalRef Text (GlobalRef Renamed)
  | UnresolvedGlobalText Text
  | UnresolvedUuid Uuid

data Lambda s = Lambda
  { location :: !(StagedLocation s)
  , param :: !(Param s)
  , body :: !(Expression s)
  , typ :: !(StagedType s)
  }

data Apply s = Apply
  { location :: !(StagedLocation s)
  , function :: !(Expression s)
  , argument :: !(Expression s)
  , typ :: !(StagedType s)
  }

data Param s = Param
  { location :: !(StagedLocation s)
  , name :: !(StagedParamName s)
  , typ :: !(StagedType s)
  }

data Binding s
  = LambdaBinding !(Param s)
  | LetBinding !(NonEmpty (Param s))
  | CaseBinding !(Param s)

data Variable s = Variable
  { location :: !(StagedLocation s)
  , name :: !(StagedVariableName s)
  , typ :: !(StagedType s)
  }

data Literal s
  = NumberLiteral (Number s)
  | TextLiteral (LiteralText s)

-- | A text.
data LiteralText s = LiteralText
  { location :: !(StagedLocation s)
  , text :: !Text
  , typ :: !(StagedType s)
  }

-- | A number.
data Number s = Number
  { location :: !(StagedLocation s)
  , number :: !SomeNumber
  , typ :: !(StagedType s)
  }

-- | A number's value.
--
-- I started with NaturalNumber in here, too. But I'm not sure that
-- natural is worth the bother on the whole. It's a small proof about
-- a number that doesn't get you much mileage.
--
-- Twitter poll: https://twitter.com/christopherdone/status/1271781700083818496
data SomeNumber
  = IntegerNumber !Integer -- ^ Any whole number.
  | DecimalNumber !Decimal
  deriving (Show, Lift, Eq, Ord)

-- | Decimal backed by an Integer with N decimal places. Precision is
-- determined at runtime.
data Decimal = Decimal
  { places :: !Natural
  , integer :: !Integer
  } deriving (Show, Lift, Eq, Ord)

--------------------------------------------------------------------------------
-- Type system types

-- Note: the reason the Record (row :: Row) is nice in PureScript is
-- because type variables can have kind :: RowKind, so only those can
-- unify. Whereas a Record x :: Type -- forall a. a -> a would unify
-- with the record, but not the row variable.

data Type s where
  VariableType :: TypeVariable s -> Type s
  PolyType :: TypeVariable Polymorphic -> Type Generalised
  ApplyType :: TypeApplication s -> Type s
  ConstantType :: TypeConstant s -> Type s
  RowType :: TypeRow s -> Type s
  RecordType :: Type s -> Type s
  VariantType :: Type s -> Type s
  ArrayType :: Type s -> Type s
  FreshType :: StagedFresh s -> Type s

type family StagedFresh s where
  StagedFresh Parsed = SourceLocation
  StagedFresh Renamed = Cursor
  StagedFresh s = Void

-- | A row type.
data TypeRow s = TypeRow
  { location :: !(StagedLocation s)
  , typeVariable :: !(Maybe (StagedRowVariable s))
  , fields :: ![Field s]
  }

type family StagedRowVariable s where
  StagedRowVariable Parsed = ()
  StagedRowVariable Renamed = ()
  StagedRowVariable s = TypeVariable s

-- | A field is a name/type pair with additional metadata.
data Field s = Field
  { location :: !(StagedLocation s)
  , name :: !FieldName
  , typ :: !(Type s)
  }

newtype FieldName = FieldName
  { unFieldName :: Text
  } deriving (Eq, Ord, Generic, Show, Lift, IsString, NFData, Hashable)

newtype TagName = TagName
  { unTagName :: Text
  } deriving (Eq, Ord, Generic, Show, Lift, Hashable)

data TypeConstant s = TypeConstant
  { location :: !(StagedLocation s)
  , name :: !TypeName
  }

data TypeApplication s = TypeApplication
  { function :: !(Type s)
  , argument :: !(Type s)
  , location :: !(StagedLocation s)
  , kind :: !Kind
  }

data StagedScheme s where
  ParsedScheme :: StagedScheme Parsed
  RenamedScheme :: StagedScheme Renamed
  FilledScheme :: StagedScheme Filled
  GeneratedScheme :: Scheme Generated -> StagedScheme Generated
  SolvedScheme :: Scheme Solved -> StagedScheme Solved
  GeneralisedScheme :: Scheme Generalised -> StagedScheme Generalised
  ResolvedScheme :: Type Generalised -> StagedScheme Resolved

data Scheme s = Scheme
  { location :: !(StagedLocation s)
  , constraints :: ![ClassConstraint s]
  , typ :: !(StagedType s)
  }

data Kind
  = TypeKind
  | FunKind Kind
            Kind
  | NatKind
  | RowKind
  deriving (Show, Lift, Eq, Ord)

data TypePoly = TypePoly
  { location :: !(StagedLocation Generated)
  , index :: !Integer
  } deriving (Show, Lift, Eq, Ord)

data TypeVariable s = TypeVariable
  { location :: !(StagedTyVarLocation s)
  , prefix :: !(StagedPrefix s)
  , index :: !Natural
  , kind :: !Kind
  }

data TypeVariablePrefix
  = LambdaParameterPrefix
  | VariablePrefix
  | ApplyPrefix
  | IntegerPrefix
  | EqualPrefix
  | ComparePrefix
  | DecimalPrefix
  | NatPrefix
  | InfixOutputPrefix
  | PolyPrefix
  | RowVarPrefix
  | VariantRowVarPrefix
  | FieldTypePrefix
  | ArrayElementPrefix
  | HolePrefix
  | IfPrefix
  | CasePrefix
  | AltPrefix
  | FreshPrefix
  | RowUnifyPrefix
  | SolverGeneratedPrefix TypeVariablePrefix
  | EarlyPrefix
  | BoundaryPrefix
  deriving (Show, Lift, Eq, Ord, Generic)
instance Hashable TypeVariablePrefix

data EqualityConstraint = EqualityConstraint
  { type1 :: Type Generated
  , type2 :: Type Generated
  , location :: !(StagedLocation Generated)
  }

data ClassConstraint s = ClassConstraint
  { className :: !ClassName
  , typ :: !(NonEmpty (Type s))
  , location :: !(StagedLocation s)
  }

data InstanceName
  = FromIntegerIntegerInstance
  | FromIntegerDecimalInstance !Natural
  | FromDecimalDecimalInstance !FromDecimalInstance
  | IntegerOpInstance !NumericBinOp
  | DecimalOpInstance !Natural !NumericBinOp
  | EqualIntegerInstance
  | EqualTextInstance
  | EqualDecimalInstance !Natural
  | CompareIntegerInstance
  | CompareTextInstance
  | CompareDecimalInstance !Natural
  deriving (Show, Lift, Eq, Ord)

data FromDecimalInstance = FromDecimalInstance
  { supersetPlaces :: !Natural
  , subsetPlaces :: !Natural
  } deriving (Show, Lift, Eq, Ord)

data TypeName
  = FunctionTypeName
  | IntegerTypeName
  | DecimalTypeName
  | TextTypeName
  | OptionTypeName
  | NatTypeName !Natural
  | VegaTypeName
  deriving (Show, Lift, Eq, Ord)

data ClassName
  = FromIntegerClassName
  | FromDecimalClassName
  | MulitplyOpClassName
  | AddOpClassName
  | SubtractOpClassName
  | DivideOpClassName
  | EqualClassName
  | CompareClassName
  deriving (Show, Lift, Eq, Ord)

--------------------------------------------------------------------------------
-- Source location information

-- | Source location of something originating in source code.
data SourceLocation = SourceLocation
  { start :: SourcePos -- ^ Deliberately lazy.
  , end :: SourcePos -- ^ Deliberately lazy.
  } deriving (Show, Lift, Eq, Ord)

-- | Position in source code.
data SourcePos = SourcePos
  { line :: Int -- ^ Deliberately lazy.
  , column :: Int -- ^ Deliberately lazy.
  , name :: FilePath -- ^ Deliberately lazy.
  } deriving (Show, Lift, Eq, Ord, Generic)

--------------------------------------------------------------------------------
-- Tree location information

-- | Location of something within a tree.
data Cursor
  = ExpressionCursor
  | LambdaBodyCursor Cursor
  | InfixOpCursor Cursor
  | InfixLeftCursor Cursor
  | InfixRightCursor Cursor
  | LetBodyCursor Cursor
  | LetBindCursor IndexInLet Cursor
  | LambdaParamCursor
  | ApplyFuncCursor Cursor
  | ApplyArgCursor Cursor
  | SignatureCursor Cursor
  | TypeCursor
  | TypeApplyCursor Cursor
  | ImplicitlyApplicationOn Cursor
  | ImplicitArgumentFor Cursor
  | BuiltIn
  | AutogeneratedCursor
  | AutoInsertedForDefaulterCursor
  | DefaultedCursor
  | SteppedCursor
  | RowFieldCursor Cursor
  | RowFieldType Cursor
  | RecordFieldCursor FieldName Cursor
  | RowFieldExpression Cursor
  | PropExpressionCursor Cursor
  | ArrayElementCursor Int Cursor
  | VariantElementCursor Cursor
  | IfCursor Cursor
  | NFCursor Int
  deriving (Show, Lift, Eq, Ord)

-- | Zero-based de Brujin indexing.
--
-- If referencing a let, then there is a sub-index for which of the
-- let bindings we're referring to.
data DeBrujinIndex
  = DeBrujinIndex !DeBrujinNesting
  | DeBrujinIndexOfLet !DeBrujinNesting
                       !IndexInLet
  deriving (Show, Lift, Eq, Ord)

deBrujinIndexNesting :: DeBrujinIndex -> DeBrujinNesting
deBrujinIndexNesting =
  \case
    DeBrujinIndex n -> n
    DeBrujinIndexOfLet n _ -> n

-- | Within a let's set of bindings, which binding are we referring
-- to?
newtype IndexInLet =
  IndexInLet Int
  deriving (Show, Lift, Eq, Ord)

-- | How many lambdas away are we from the binding lambda?
newtype DeBrujinNesting =
  DeBrujinNesting Int
  deriving (Show, Lift, Eq, Ord, Num)

-- | Hash used for CAS referencing.
newtype Hash = Hash SHA512
  deriving (Show, Lift, Eq, Ord)

-- | A global reference, which may either be a hash, or a built-in
-- thing.
data GlobalRef s where
  HashGlobal :: Hash -> GlobalRef s
  FromIntegerGlobal :: GlobalRef s
  FromDecimalGlobal :: GlobalRef s
  NumericBinOpGlobal :: NumericBinOp -> GlobalRef s
  EqualGlobal :: !Equality -> GlobalRef s
  CompareGlobal :: !Comparison -> GlobalRef s
  InstanceGlobal :: !InstanceName -> GlobalRef Resolved
  FunctionGlobal :: !Function -> GlobalRef s

data Comparison
  = LessThan
  | GreaterThan
  | GreaterEqualTo
  | LessEqualTo
  deriving (Show, Lift, Eq, Ord)

data Equality
  = Equal
  | NotEqual
  deriving (Show, Lift, Eq, Ord)

-- | Numeric binary operator.
data NumericBinOp
  = MulitplyOp
  | AddOp
  | SubtractOp
  | DivideOp
  deriving (Show, Lift, Eq, Ord)

data Function
  -- Done:
  = MapFunction
  | FilterFunction
  | SumFunction
  | AverageFunction
  | VegaFunction
  | NullFunction
  | LengthFunction
  | DistinctFunction
  | MinimumFunction
  | MaximumFunction
  | SortFunction
  | FindFunction
  | AllFunction
  | AnyFunction
  | FromOkFunction
  | ConcatFunction
  -- TODO:
  | AndFunction
  | OrFunction
  deriving (Show, Lift, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Stages

data Parsed
data Parsed2
data Renamed
data Filled
data Desugared
data Generated
data Solved
data Generalised
data Polymorphic
data Resolved

--------------------------------------------------------------------------------
-- Families

type family StagedForm s where
  StagedForm Resolved = TermForm
  StagedForm s = ()

type family StagedLocation s where
  StagedLocation Parsed = SourceLocation
  StagedLocation Parsed2 = FlatParse.Pos
  StagedLocation Renamed = Cursor
  StagedLocation Filled = Cursor
  StagedLocation Desugared = Cursor
  StagedLocation Generated = Cursor
  StagedLocation Solved = Cursor
  StagedLocation Generalised = Cursor
  StagedLocation Polymorphic = Cursor
  StagedLocation Resolved = Cursor

type family StagedTyVarLocation s where
  StagedTyVarLocation Parsed = SourceLocation
  StagedTyVarLocation Parsed2 = SourceLocation
  StagedTyVarLocation Renamed = Cursor
  StagedTyVarLocation Filled = Cursor
  StagedTyVarLocation Desugared = Cursor
  StagedTyVarLocation Generated = Cursor
  StagedTyVarLocation Solved = Cursor
  StagedTyVarLocation Generalised = Cursor
  StagedTyVarLocation Polymorphic = ()
  StagedTyVarLocation Resolved = Cursor

type family StagedPrefix s where
  StagedPrefix Parsed = TypeVariablePrefix
  StagedPrefix Parsed2 = TypeVariablePrefix
  StagedPrefix Renamed = TypeVariablePrefix
  StagedPrefix Filled = TypeVariablePrefix
  StagedPrefix Desugared = TypeVariablePrefix
  StagedPrefix Generated = TypeVariablePrefix
  StagedPrefix Solved = TypeVariablePrefix
  StagedPrefix Generalised = TypeVariablePrefix
  StagedPrefix Resolved = TypeVariablePrefix
  StagedPrefix Polymorphic = ()

type family StagedType s where
  StagedType Parsed = Maybe (Type Parsed)
  StagedType Parsed2 = ()
  StagedType Renamed = Maybe (Type Renamed)
  StagedType Filled = Maybe (Type Renamed)
  StagedType Desugared = Maybe (Type Renamed)
  StagedType Generated = Type Generated
  StagedType Solved = Type Solved
  StagedType Generalised = Type Generalised
  StagedType Resolved = Type Generalised
  StagedType Polymorphic = Type Polymorphic

type family StagedParamName s where
  StagedParamName Parsed = Text
  StagedParamName Parsed2 = Text
  StagedParamName Renamed = ()
  StagedParamName Filled = ()
  StagedParamName Desugared = ()
  StagedParamName Generated = ()
  StagedParamName Solved = ()
  StagedParamName Generalised = ()
  StagedParamName Resolved = ()

type family StagedVariableName s where
  StagedVariableName Parsed = Text
  StagedVariableName Parsed2 = Text
  StagedVariableName Renamed = DeBrujinIndex
  StagedVariableName Filled = DeBrujinIndex
  StagedVariableName Desugared = DeBrujinIndex
  StagedVariableName Generated = DeBrujinIndex
  StagedVariableName Solved = DeBrujinIndex
  StagedVariableName Generalised = DeBrujinIndex
  StagedVariableName Resolved = DeBrujinIndex

type family StagedGlobalName s where
  StagedGlobalName Parsed = ParsedGlobal
  StagedGlobalName Parsed2 = ParsedGlobal
  StagedGlobalName Renamed = IncompleteGlobalRef
  StagedGlobalName Filled = GlobalRef Renamed
  StagedGlobalName Desugared = GlobalRef Renamed
  StagedGlobalName Generated = GlobalRef Generated
  StagedGlobalName Solved = GlobalRef Solved
  StagedGlobalName Generalised = GlobalRef Generalised
  StagedGlobalName Resolved = GlobalRef Resolved

type family StagedOp s where
  StagedOp Parsed = Global Parsed
  StagedOp Parsed2 = Global Parsed
  StagedOp Renamed = Global Renamed
  StagedOp Filled = Global Filled
  StagedOp Desugared = Global Desugared
  StagedOp Generated = Global Generated
  StagedOp Solved = Global Solved
  StagedOp Generalised = Global Generalised
  StagedOp Resolved = Expression Resolved
