{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Inflex.Instances ()
import           Inflex.Optics
import           Inflex.Parser
import           Inflex.Types
import           Optics

--------------------------------------------------------------------------------
-- Renamer types

data RenameError =
  MissingVariable [Param Parsed] (Variable Parsed)
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

data Env = Env
  { cursor :: !CursorBuilder
  , scope :: ![Param Parsed]
  }

data IsRenamed a = IsRenamed
  { thing :: a
  , mappings :: Map Cursor SourceLocation
  } deriving (Show, Eq)

$(makeLensesWith (inflexRules ['cursor, 'scope]) ''Env)

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
             (runValidateT
                (runRenamer
                   (renameExpression
                      (Env {cursor = id, scope = mempty})
                      expression)))
             mempty
      in fmap (\thing -> IsRenamed {thing, mappings}) result)

--------------------------------------------------------------------------------
-- Renamers

renameExpression :: Env -> Expression Parsed -> Renamer (Expression Renamed)
renameExpression env =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (renameLiteral env literal)
    LambdaExpression lambda ->
      fmap LambdaExpression (renameLambda env lambda)
    ApplyExpression apply -> fmap ApplyExpression (renameApply env apply)
    VariableExpression variable ->
      fmap VariableExpression (renameVariable env variable)

renameLiteral :: Env -> Literal Parsed -> Renamer (Literal Renamed)
renameLiteral env =
  \case
    NumberLiteral number -> fmap NumberLiteral (renameNumber env number)

renameNumber :: Env -> Number Parsed -> Renamer (Number Renamed)
renameNumber Env{cursor} Number {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  pure Number {location = final, ..}

renameLambda :: Env -> Lambda Parsed -> Renamer (Lambda Renamed)
renameLambda env@Env {cursor} Lambda {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  param' <- renameParam env param
  body' <-
    renameExpression
      (over envScopeL (param :) (over envCursorL (. LambdaBodyCursor) env))
      body
  pure Lambda {body = body', location = final, param = param', ..}

renameApply :: Env -> Apply Parsed -> Renamer (Apply Renamed)
renameApply env@Env {cursor} Apply {..} = do
  function' <-
    renameExpression (over envCursorL (. ApplyFuncCursor) env) function
  argument' <-
    renameExpression (over envCursorL (. ApplyArgCursor) env) argument
  final <- finalizeCursor cursor ExpressionCursor location
  pure Apply {function = function', argument = argument', location = final, ..}

renameVariable :: Env -> Variable Parsed -> Renamer (Variable Renamed)
renameVariable Env {scope, cursor} variable@Variable {name, location} =
  case findIndex (\Param {name = name'} -> name' == name) scope of
    Nothing -> Renamer (refute (pure (MissingVariable scope variable)))
    Just index -> do
      final <- finalizeCursor cursor ExpressionCursor location
      pure (Variable {location = final, name = DeBrujinIndex index, typ = ()})

renameParam :: Env -> Param Parsed -> Renamer (Param Renamed)
renameParam Env{cursor} Param {..} = do
  final <- finalizeCursor cursor LambdaParamCursor location
  pure Param {name = (), location = final, ..}

--------------------------------------------------------------------------------
-- Cursor operations

finalizeCursor :: CursorBuilder -> Cursor -> StagedLocation Parsed -> Renamer Cursor
finalizeCursor cursor finalCursor loc = do
  modify (M.insert final loc)
  pure final
  where final = cursor finalCursor
