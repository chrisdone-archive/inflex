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

--------------------------------------------------------------------------------
-- Top-level

renameText ::
     FilePath
  -> Text
  -> Either ParseRenameError (Expression Renamed, Map Cursor SourceLocation)
renameText fp text = do
  expression <- first ParserErrored (parseText fp text)
  first
    RenamerErrors
    (let (result, state') =
           runState
             (runValidateT (runRenamer (renameExpression id expression)))
             mempty
      in fmap (, state') result)

--------------------------------------------------------------------------------
-- Renamers

renameExpression :: CursorBuilder -> Expression Parsed -> Renamer (Expression Renamed)
renameExpression cursor =
  \case
    LiteralExpression literal ->
      fmap LiteralExpression (renameLiteral cursor literal)
    LambdaExpression lambda ->
      fmap LambdaExpression (renameLambda cursor lambda)

renameLiteral :: CursorBuilder -> Literal Parsed -> Renamer (Literal Renamed)
renameLiteral cursor =
  \case
    IntegerLiteral integery -> fmap IntegerLiteral (renameIntegery cursor integery)

renameIntegery :: CursorBuilder -> Integery Parsed -> Renamer (Integery Renamed)
renameIntegery cursor Integery {..} = do
  final <- finalizeCursor cursor location
  pure Integery {location = final, ..}

renameLambda :: CursorBuilder -> Lambda Parsed -> Renamer (Lambda Renamed)
renameLambda cursor Lambda {..} = do
  final <- finalizeCursor cursor location
  body' <- renameExpression (cursor . InLambdaCursor) body
  pure Lambda {body = body', location = final, ..}

--------------------------------------------------------------------------------
-- Cursor operations

finalizeCursor :: CursorBuilder -> StagedLocation Parsed -> Renamer Cursor
finalizeCursor cursor loc = do
  modify (M.insert final loc)
  pure final
  where final = cursor FinalCursor
