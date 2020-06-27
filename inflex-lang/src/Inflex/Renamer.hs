{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
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

data RenameError
  = MissingVariable [Param Parsed]
                    (Variable Parsed)
  | MissingGlobal (Map Text GlobalRef)
                  (Global Parsed)
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
  , globals :: !(Map Text GlobalRef)
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
                      (Env {globals = wiredInGlobals, cursor = id, scope = mempty})
                      expression)))
             mempty
      in fmap (\thing -> IsRenamed {thing, mappings}) result)

--------------------------------------------------------------------------------
-- Wired-in

wiredInGlobals :: Map Text GlobalRef
wiredInGlobals =
  M.fromList
    [("fromInteger", FromIntegerGlobal), ("fromDecimal", FromDecimalGlobal)]

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
    GlobalExpression global ->
      fmap GlobalExpression (renameGlobal env global)

renameLiteral :: Env -> Literal Parsed -> Renamer (Literal Renamed)
renameLiteral env =
  \case
    NumberLiteral number -> fmap NumberLiteral (renameNumber env number)

renameNumber :: Env -> Number Parsed -> Renamer (Number Renamed)
renameNumber env@Env{cursor} Number {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  typ' <- renameSignature env typ
  pure Number {location = final, typ=typ', ..}

renameLambda :: Env -> Lambda Parsed -> Renamer (Lambda Renamed)
renameLambda env@Env {cursor} Lambda {..} = do
  final <- finalizeCursor cursor ExpressionCursor location
  param' <- renameParam env param
  body' <-
    renameExpression
      (over envScopeL (param :) (over envCursorL (. LambdaBodyCursor) env))
      body
  typ' <- renameSignature env typ
  pure Lambda {body = body', location = final, param = param', typ=typ', ..}

renameApply :: Env -> Apply Parsed -> Renamer (Apply Renamed)
renameApply env@Env {cursor} Apply {..} = do
  function' <-
    renameExpression (over envCursorL (. ApplyFuncCursor) env) function
  argument' <-
    renameExpression (over envCursorL (. ApplyArgCursor) env) argument
  final <- finalizeCursor cursor ExpressionCursor location
  typ' <- renameSignature env typ
  pure Apply {function = function', argument = argument', location = final, typ = typ'}

renameVariable :: Env -> Variable Parsed -> Renamer (Variable Renamed)
renameVariable env@Env {scope, cursor} variable@Variable {name, location, typ} =
  case findIndex (\Param {name = name'} -> name' == name) scope of
    Nothing -> Renamer (refute (pure (MissingVariable scope variable)))
    Just index -> do
      final <- finalizeCursor cursor ExpressionCursor location
      typ' <- renameSignature env typ
      pure (Variable {location = final, name = DeBrujinIndex index, typ=typ'})

renameParam :: Env -> Param Parsed -> Renamer (Param Renamed)
renameParam env@Env{cursor} Param {..} = do
  final <- finalizeCursor cursor LambdaParamCursor location
  typ' <- renameSignature env typ
  pure Param {name = (), location = final, typ = typ'}

renameGlobal :: Env -> Global Parsed -> Renamer (Global Renamed)
renameGlobal Env {cursor, globals} global@Global {name, location} =
  case M.lookup name globals of
    Nothing -> Renamer (refute (pure (MissingGlobal globals global)))
    Just globalRef -> do
      final <- finalizeCursor cursor ExpressionCursor location
      pure Global {location = final, name = globalRef, scheme = RenamedScheme}

renameSignature :: Env -> Maybe (Type Parsed) -> Renamer (Maybe (Type Renamed))
renameSignature env = maybe (pure Nothing) (fmap Just . renameType env)

renameType :: Env -> Type Parsed -> Renamer (Type Renamed)
renameType env =
  \case
    VariableType typeVariable ->
      fmap VariableType (renameTypeVariable env typeVariable)
    ApplyType typeApplication ->
      fmap ApplyType (renameTypeApplication env typeApplication)
    ConstantType typeConstant ->
      fmap ConstantType (renameTypeConstant env typeConstant)

renameTypeConstant :: Env -> TypeConstant Parsed -> Renamer (TypeConstant Renamed)
renameTypeConstant Env{cursor} TypeConstant {..} = do
  final <- finalizeCursor cursor TypeCursor location
  pure TypeConstant {location = final, ..}

renameTypeApplication :: Env -> TypeApplication Parsed -> Renamer (TypeApplication Renamed)
renameTypeApplication env@Env {cursor} TypeApplication {function, argument, ..} = do
  function' <- renameType (over envCursorL (. TypeApply) env) function
  argument' <- renameType (over envCursorL (. TypeApply) env) argument
  final <- finalizeCursor cursor TypeCursor location
  pure
    TypeApplication
      {function = function', argument = argument', location = final, ..}

renameTypeVariable :: Env -> TypeVariable Parsed -> Renamer (TypeVariable Renamed)
renameTypeVariable Env{cursor} TypeVariable {..} = do
  final <- finalizeCursor cursor TypeCursor location
  pure TypeVariable {location = final, ..}

--------------------------------------------------------------------------------
-- Cursor operations

finalizeCursor :: CursorBuilder -> Cursor -> StagedLocation Parsed -> Renamer Cursor
finalizeCursor cursor finalCursor loc = do
  modify (M.insert final loc)
  pure final
  where final = cursor finalCursor
