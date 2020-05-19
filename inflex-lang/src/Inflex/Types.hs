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
  VariableType :: TypeVariablePrefix -> Integer -> Type Generated
  ApplyType :: Type s -> Type s -> Type s
  ConstantType :: TypeName -> Type s

data TypeVariablePrefix
  = IntegeryPrefix
  | LambdaParameterPrefix
  deriving (Show, Eq, Ord)

data ClassConstraint s = ClassConstraint
  { className :: !ClassName
  , types :: !(NonEmpty (Type s))
  }

data TypeName =
  FunctionTypeName
  deriving (Show, Eq, Ord)

data ClassName =
  FromIntegerClassName
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Location information

-- | A location of a thing.
data Location = Location
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
  StagedLocation Parsed = Location
  StagedLocation Renamed = Location
  StagedLocation Generated = Location

type family StagedType s where
  StagedType Parsed = ()
  StagedType Renamed = ()
  StagedType Generated = Type Generated
