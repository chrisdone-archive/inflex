{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Renamer for Inflex language.

module Inflex.Renamer
  ( renameText
  , IsRenamed(..)
  , RenameError(..)
  , ParseRenameError(..)
  ) where

import           Control.Monad.State
import           Control.Monad.Validate
import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Inflex.Parser
import           Inflex.Types

--------------------------------------------------------------------------------
-- Renamer types

data RenameError = RenameError
  deriving (Show, Eq)

newtype Renamer a = Renamer
  { runRenamer :: ValidateT (NonEmpty RenameError) (State (Map Cursor SourceLocation)) a
  } deriving ( Functor
             , Applicative
             , MonadState (Map Cursor SourceLocation)
             , Monad
             )

data ParseRenameError
  = RenamerErrors (NonEmpty RenameError)
  | ParserErrored RenameParseError
  deriving (Show, Eq)

type CursorBuilder = Cursor -> Cursor

data IsRenamed a = IsRenamed
  { thing :: a
  , mappings :: Map Cursor SourceLocation
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level

renameText ::
     FilePath
  -> Text
  -> Either ParseRenameError (IsRenamed (Expression Renamed))
renameText fp text = do
  expression <- first ParserErrored (parseText fp text)
  first
    RenamerErrors
    (let (result, mappings) =
           runState
             (runValidateT (runRenamer (renameExpression id expression)))
             mempty
      in fmap (\thing -> IsRenamed {thing, mappings}) result)

--------------------------------------------------------------------------------
-- Renamers

renameExpression :: CursorBuilder -> Expression Parsed -> Renamer (Expression Renamed)
renameExpression cursor =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (renameLiteral cursor literal)
    LambdaExpression lambda ->
      fmap LambdaExpression (renameLambda cursor lambda)
    ApplyExpression apply -> fmap ApplyExpression (renameApply cursor apply)
    VariableExpression variable ->
      fmap VariableExpression (renameVariable cursor variable)

renameLiteral :: CursorBuilder -> Literal Parsed -> Renamer (Literal Renamed)
renameLiteral cursor =
  \case
    IntegerLiteral integery -> fmap IntegerLiteral (renameIntegery cursor integery)

renameIntegery :: CursorBuilder -> Integery Parsed -> Renamer (Integery Renamed)
renameIntegery cursor Integery {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  pure Integery {location = final, ..}

renameLambda :: CursorBuilder -> Lambda Parsed -> Renamer (Lambda Renamed)
renameLambda cursor Lambda {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  param' <- renameParam cursor param
  body' <- renameExpression (cursor . LambdaBodyCursor) body
  pure Lambda {body = body', location = final, param = param', ..}

renameApply :: CursorBuilder -> Apply Parsed -> Renamer (Apply Renamed)
renameApply cursor Apply {..} = do
  function' <- renameExpression (cursor . ApplyFuncCursor) function
  argument' <- renameExpression (cursor . ApplyArgCursor) argument
  final <- finalizeCursor cursor ExpressionCursor location
  pure Apply {function = function', argument = argument', location = final, ..}

renameVariable :: CursorBuilder -> Variable Parsed -> Renamer (Variable Renamed)
renameVariable = error "TODO: rename variable"

renameParam :: CursorBuilder -> Param Parsed -> Renamer (Param Renamed)
renameParam cursor Param {..} = do
  final <- finalizeCursor cursor LambdaParamCursor location
  pure Param {name = (), location = final, ..}

--------------------------------------------------------------------------------
-- Cursor operations

finalizeCursor :: CursorBuilder -> Cursor -> StagedLocation Parsed -> Renamer Cursor
finalizeCursor cursor finalCursor loc = do
  modify (M.insert final loc)
  pure final
  where final = cursor finalCursor
