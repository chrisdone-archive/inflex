{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Project-wide shared types.

module Inflex.Types where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import           GHC.Generics

--------------------------------------------------------------------------------
-- AST types

data Expression s =
    LiteralExpression (Literal s)
  | LambdaExpression (Lambda s)

data Lambda s = Lambda
  { location :: !(StagedLocation s)
  , param :: !(Param s)
  , body :: !(Expression s)
  , typ :: !(StagedType s)
  }

data Param s = Param
  { location :: !(StagedLocation s)
  , name :: !(StagedName s)
  , typ :: !(StagedType s)
  }

data Literal s =
  IntegerLiteral (Integery s)

data Integery s = Integery
  { location :: !(StagedLocation s)
  , integer :: !Integer
  , typ :: !(StagedType s)
  }

--------------------------------------------------------------------------------
-- Type system types

data Type s where
  VariableType :: (TypeVariableStage s ~ s) => TypeVariable s -> Type s
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
  { location :: !(StagedLocation s)
  , prefix :: !TypeVariablePrefix
  , index :: !Integer
  }

data TypeVariablePrefix
  = IntegeryPrefix
  | LambdaParameterPrefix
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

data TypeName =
  FunctionTypeName
  deriving (Show, Eq, Ord)

data ClassName =
  FromIntegerClassName
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
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Stages

data Parsed
data Renamed
data Generated
data Solved

--------------------------------------------------------------------------------
-- Families

type family StagedLocation s where
  StagedLocation Parsed = SourceLocation
  StagedLocation Renamed = Cursor
  StagedLocation Generated = Cursor
  StagedLocation Solved = Cursor

type family StagedType s where
  StagedType Parsed = ()
  StagedType Renamed = ()
  StagedType Generated = Type Generated
  StagedType Solved = Type Solved

type family StagedName s where
  StagedName Parsed = Text
  StagedName Renamed = ()
  StagedName Generated = ()
  StagedName Solved = ()

type family TypeVariableStage s where
  TypeVariableStage Solved = Solved
  TypeVariableStage Generated = Generated
