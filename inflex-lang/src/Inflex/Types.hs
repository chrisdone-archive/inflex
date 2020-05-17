{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

-- | Project-wide shared types.

module Inflex.Types where

import GHC.Generics

--------------------------------------------------------------------------------
-- AST types

data Expression s =
  LiteralExpression (Literal s)

data Literal s =
  IntegerLiteral (Integery s)

data Integery s = Integery
  { location :: !(StagedLocation s)
  , integer :: !Integer
  , typ :: !(StagedType s)
  }

--------------------------------------------------------------------------------
-- Type system types

data GeneratedType =
  VariableGeneratedType !TypeVariablePrefix !Integer
  deriving (Show, Eq, Ord)

data TypeVariablePrefix =
  IntegeryPrefix
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
  StagedType Generated = GeneratedType
