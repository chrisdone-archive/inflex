{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Project-wide shared types.

module Inflex.Types where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics
import Inflex.Types.SHA512
import Numeric.Natural

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
  } deriving (Show, Eq, Ord, Functor)

newtype Uuid = Uuid Text
 deriving (Eq, Ord, Show)


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

data IncompleteGlobalRef
  = GlobalRef (GlobalRef Renamed)
  | UnresolvedGlobal Text

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

data Variable s = Variable
  { location :: !(StagedLocation s)
  , name :: !(StagedVariableName s)
  , typ :: !(StagedType s)
  }

data Literal s =
  NumberLiteral (Number s)

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
  deriving (Show, Eq, Ord)

-- | Decimal backed by an Integer with N decimal places. Precision is
-- determined at runtime.
data Decimal = Decimal
  { places :: !Natural
  , integer :: !Integer
  } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Type system types

data Type s where
  VariableType :: TypeVariable s -> Type s
  PolyType :: TypeVariable Polymorphic -> Type Generalised
  ApplyType :: TypeApplication s -> Type s
  ConstantType :: TypeConstant s -> Type s
  RowType :: TypeRow s -> Type s

-- | A row type.
data TypeRow s = TypeRow
  { typeVariable :: !(Maybe (TypeVariable s))
  , fields :: ![Field s]
  }

-- | A field is a name/type pair with additional metadata.
data Field s = Field
  { name :: !FieldName
  , typ :: !(Type s)
  }

newtype FieldName = FieldName
  { unFieldName :: Text
  } deriving (Eq, Ord, Generic, Show)

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
  deriving (Show, Eq, Ord)

data TypePoly = TypePoly
  { location :: !(StagedLocation Generated)
  , index :: !Integer
  } deriving (Show, Eq, Ord)

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
  | DecimalPrefix
  | NatPrefix
  | InfixOutputPrefix
  | PolyPrefix
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

data FromDecimalInstance = FromDecimalInstance
  { supersetPlaces :: !Natural
  , subsetPlaces :: !Natural
  } deriving (Show, Eq, Ord)

data TypeName
  = FunctionTypeName
  | IntegerTypeName
  | DecimalTypeName
  | TextTypeName
  | OptionTypeName
  | NatTypeName !Natural
  deriving (Show, Eq, Ord)

data ClassName
  = FromIntegerClassName
  | FromDecimalClassName
  | MulitplyOpClassName
  | AddOpClassName
  | SubtractOpClassName
  | DivideOpClassName
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Source location information

-- | Source location of something originating in source code.
data SourceLocation = SourceLocation
  { start :: !SourcePos
  , end :: !SourcePos
  } deriving (Show, Eq, Ord)

-- | Position in source code.
data SourcePos = SourcePos
  { line :: !Int
  , column :: !Int
  , name :: !FilePath
  } deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord)

-- | Zero-based de Brujin indexing.
--
-- If referencing a let, then there is a sub-index for which of the
-- let bindings we're referring to.
data DeBrujinIndex
  = DeBrujinIndex !DeBrujinNesting
  | DeBrujinIndexOfLet !DeBrujinNesting
                       !IndexInLet
  deriving (Show, Eq, Ord)

deBrujinIndexNesting :: DeBrujinIndex -> DeBrujinNesting
deBrujinIndexNesting =
  \case
    DeBrujinIndex n -> n
    DeBrujinIndexOfLet n _ -> n

-- | Within a let's set of bindings, which binding are we referring
-- to?
newtype IndexInLet =
  IndexInLet Int
  deriving (Show, Eq, Ord)

-- | How many lambdas away are we from the binding lambda?
newtype DeBrujinNesting =
  DeBrujinNesting Int
  deriving (Show, Eq, Ord, Num)

-- | Hash used for CAS referencing.
newtype Hash = Hash SHA512
  deriving (Show, Eq, Ord)

-- | A global reference, which may either be a hash, or a built-in
-- thing.
data GlobalRef s where
  HashGlobal :: Hash -> GlobalRef s
  FromIntegerGlobal :: GlobalRef s
  FromDecimalGlobal :: GlobalRef s
  NumericBinOpGlobal :: NumericBinOp -> GlobalRef s
  InstanceGlobal :: !InstanceName -> GlobalRef Resolved

-- | Numeric binary operator.
data NumericBinOp
  = MulitplyOp
  | AddOp
  | SubtractOp
  | DivideOp
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Stages

data Parsed
data Renamed
data Filled
data Generated
data Solved
data Generalised
data Polymorphic
data Resolved

--------------------------------------------------------------------------------
-- Families

type family StagedLocation s where
  StagedLocation Parsed = SourceLocation
  StagedLocation Renamed = Cursor
  StagedLocation Filled = Cursor
  StagedLocation Generated = Cursor
  StagedLocation Solved = Cursor
  StagedLocation Generalised = Cursor
  StagedLocation Polymorphic = Cursor
  StagedLocation Resolved = Cursor

type family StagedTyVarLocation s where
  StagedTyVarLocation Parsed = SourceLocation
  StagedTyVarLocation Renamed = Cursor
  StagedTyVarLocation Filled = Cursor
  StagedTyVarLocation Generated = Cursor
  StagedTyVarLocation Solved = Cursor
  StagedTyVarLocation Generalised = Cursor
  StagedTyVarLocation Polymorphic = ()
  StagedTyVarLocation Resolved = Cursor

type family StagedPrefix s where
  StagedPrefix Parsed = TypeVariablePrefix
  StagedPrefix Renamed = TypeVariablePrefix
  StagedPrefix Filled = TypeVariablePrefix
  StagedPrefix Generated = TypeVariablePrefix
  StagedPrefix Solved = TypeVariablePrefix
  StagedPrefix Generalised = TypeVariablePrefix
  StagedPrefix Resolved = TypeVariablePrefix
  StagedPrefix Polymorphic = ()

type family StagedType s where
  StagedType Parsed = Maybe (Type Parsed)
  StagedType Renamed = Maybe (Type Renamed)
  StagedType Filled = Maybe (Type Renamed)
  StagedType Generated = Type Generated
  StagedType Solved = Type Solved
  StagedType Generalised = Type Generalised
  StagedType Resolved = Type Generalised
  StagedType Polymorphic = Type Polymorphic

type family StagedParamName s where
  StagedParamName Parsed = Text
  StagedParamName Renamed = ()
  StagedParamName Filled = ()
  StagedParamName Generated = ()
  StagedParamName Solved = ()
  StagedParamName Generalised = ()
  StagedParamName Resolved = ()

type family StagedVariableName s where
  StagedVariableName Parsed = Text
  StagedVariableName Renamed = DeBrujinIndex
  StagedVariableName Filled = DeBrujinIndex
  StagedVariableName Generated = DeBrujinIndex
  StagedVariableName Solved = DeBrujinIndex
  StagedVariableName Generalised = DeBrujinIndex
  StagedVariableName Resolved = DeBrujinIndex

type family StagedGlobalName s where
  StagedGlobalName Parsed = Text
  StagedGlobalName Renamed = IncompleteGlobalRef
  StagedGlobalName Filled = GlobalRef Renamed
  StagedGlobalName Generated = GlobalRef Generated
  StagedGlobalName Solved = GlobalRef Solved
  StagedGlobalName Generalised = GlobalRef Generalised
  StagedGlobalName Resolved = GlobalRef Resolved

type family StagedOp s where
  StagedOp Parsed = Global Parsed
  StagedOp Renamed = Global Renamed
  StagedOp Filled = Global Filled
  StagedOp Generated = Global Generated
  StagedOp Solved = Global Solved
  StagedOp Generalised = Global Generalised
  StagedOp Resolved = Expression Resolved
