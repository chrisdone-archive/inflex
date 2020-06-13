{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Project-wide shared types.

module Inflex.Types where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Generics
import Numeric.Natural

--------------------------------------------------------------------------------
-- AST types

data Expression s =
    LiteralExpression !(Literal s)
  | LambdaExpression !(Lambda s)
  | ApplyExpression !(Apply s)
  | VariableExpression !(Variable s)

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
data SomeNumber
  = IntegerNumber Integer -- ^ Any whole number.
  | DecimalNumber Integer Natural -- ^ A numerator and denominator.
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Type system types

data Type s where
  VariableType :: TypeVariable s -> Type s
  PolyType :: TypeVariable Polymorphic -> Type Generalised
  ApplyType :: TypeApplication s -> Type s
  ConstantType :: TypeConstant s -> Type s

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

data TypeSignature = TypeSignature
  { location :: !(StagedLocation Solved)
  , variables :: ![Kind]
  , constraints :: ![ClassConstraint Solved]
  , typ :: !(Type Solved)
  }

data Kind
  = TypeKind
  | FunKind Kind
            Kind
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
  = NumberPrefix
  | LambdaParameterPrefix
  | VariablePrefix
  | ApplyPrefix
  deriving (Show, Eq, Ord)

data EqualityConstraint = EqualityConstraint
  { type1 :: Type Generated
  , type2 :: Type Generated
  , location :: !(StagedLocation Generated)
  }

data ClassConstraint s = ClassConstraint
  { className :: !ClassName
  , types :: !(NonEmpty (Type s))
  , location :: !(StagedLocation s)
  }

data TypeName
  = FunctionTypeName
  | NaturalTypeName
  | IntegerTypeName
  | DecimalTypeName Natural
  | TextTypeName
  | OptionTypeName
  deriving (Show, Eq, Ord)

data ClassName
  = FromNaturalClassName
  | FromIntegerClassName
  | FromDecimalClassName Natural
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
  | LambdaParamCursor
  | ApplyFuncCursor Cursor
  | ApplyArgCursor Cursor
  deriving (Show, Eq, Ord)

newtype DeBrujinIndex =
  DeBrujinIndex Int
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Stages

data Parsed
data Renamed
data Generated
data Solved
data Generalised
data Polymorphic

--------------------------------------------------------------------------------
-- Families

type family StagedLocation s where
  StagedLocation Parsed = SourceLocation
  StagedLocation Renamed = Cursor
  StagedLocation Generated = Cursor
  StagedLocation Solved = Cursor
  StagedLocation Generalised = Cursor
  StagedLocation Polymorphic = Cursor

type family StagedTyVarLocation s where
  StagedTyVarLocation Parsed = SourceLocation
  StagedTyVarLocation Renamed = Cursor
  StagedTyVarLocation Generated = Cursor
  StagedTyVarLocation Solved = Cursor
  StagedTyVarLocation Generalised = Cursor
  StagedTyVarLocation Polymorphic = ()

type family StagedPrefix s where
  StagedPrefix Parsed = TypeVariablePrefix
  StagedPrefix Renamed = TypeVariablePrefix
  StagedPrefix Generated = TypeVariablePrefix
  StagedPrefix Solved = TypeVariablePrefix
  StagedPrefix Generalised = TypeVariablePrefix
  StagedPrefix Polymorphic = ()

type family StagedType s where
  StagedType Parsed = ()
  StagedType Renamed = ()
  StagedType Generated = Type Generated
  StagedType Solved = Type Solved
  StagedType Generalised = Type Generalised

type family StagedParamName s where
  StagedParamName Parsed = Text
  StagedParamName Renamed = ()
  StagedParamName Generated = ()
  StagedParamName Solved = ()
  StagedParamName Generalised = ()

type family StagedVariableName s where
  StagedVariableName Parsed = Text
  StagedVariableName Renamed = DeBrujinIndex
  StagedVariableName Generated = DeBrujinIndex
  StagedVariableName Solved = DeBrujinIndex
  StagedVariableName Generalised = DeBrujinIndex
