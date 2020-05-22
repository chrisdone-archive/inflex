{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Project-wide shared types.

module Inflex.Types where

import           Data.List.NonEmpty (NonEmpty(..))
import           GHC.Generics

--------------------------------------------------------------------------------
-- AST types

data Expression s =
    LiteralExpression (Literal s)
  | LambdaExpression (Lambda s)

data Lambda s = Lambda
  { location :: !(StagedLocation s)
  , body :: Expression s
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
  VariableType :: TypeVariable -> Type Generated
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

data TypeVariable = TypeVariable
  { location :: !(StagedLocation Generated)
  , prefix :: !TypeVariablePrefix
  , index :: !Integer
  } deriving (Show, Eq, Ord)

data TypeVariablePrefix
  = IntegeryPrefix
  | LambdaParameterPrefix
  deriving (Show, Eq, Ord)

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

-- | A location of a thing.
data SourceLocation = SourceLocation
  { start :: !SourcePos
  , end :: !SourcePos
  } deriving (Show, Eq, Ord)

-- | Position in source.
data SourcePos = SourcePos
  { line :: !Int
  , column :: !Int
  , name :: !FilePath
  } deriving (Show, Eq, Ord, Generic)

--------------------------------------------------------------------------------
-- Stages

data Parsed
data Renamed
data Generated

--------------------------------------------------------------------------------
-- Families

type family StagedLocation s where
  StagedLocation Parsed = SourceLocation
  StagedLocation Renamed = SourceLocation
  StagedLocation Generated = SourceLocation

type family StagedType s where
  StagedType Parsed = ()
  StagedType Renamed = ()
  StagedType Generated = Type Generated
