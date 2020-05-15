{-# LANGUAGE TypeFamilies #-}

-- | Stages of compilation.

module Inflex.Stages where

import Inflex.Lexer

--------------------------------------------------------------------------------
-- Stages

data Parsed
data Renamed
data Inferred

--------------------------------------------------------------------------------
-- Families

type family StagedLocation s where
  StagedLocation Parsed = Location
  StagedLocation Renamed = Location
  StagedLocation Inferred = Location
