{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Renamer types for Inflex language.

module Inflex.Types.Renamer where

import           Control.Monad.State
import           Control.Monad.Validate
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import           Inflex.Instances ()
import           Inflex.Optics
import           Inflex.Parser
import           Inflex.Types
import           Optics

data RenameError
  = BUG_MissingVariable [Binding Parsed]
                        (Map Text (GlobalRef Renamed))
                        (Variable Parsed)
  | BUG_UnknownOperatorName Text
  deriving (Show, Eq)

newtype Renamer a = Renamer
  { runRenamer :: ValidateT (NonEmpty RenameError) (State (Map Cursor SourceLocation, Set Text)) a
  } deriving ( Functor
             , Applicative
             , MonadState (Map Cursor SourceLocation, Set Text)
             , Monad
             )

data ParseRenameError
  = RenamerErrors (NonEmpty RenameError)
  | ParserErrored LexParseError
  deriving (Show, Eq)

type CursorBuilder = Cursor -> Cursor

data Env = Env
  { cursor :: !CursorBuilder
  , scope :: ![Binding Parsed]
  , globals :: !(Map Text (GlobalRef Renamed))
  }

data IsRenamed a = IsRenamed
  { thing :: a
  , mappings :: Map Cursor SourceLocation
  , unresolvedGlobals :: Set Text
  } deriving (Show, Eq)

$(makeLensesWith (inflexRules ['cursor, 'scope]) ''Env)
