{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Loading a document of cells.

module Inflex.Document where

import           Data.Bifunctor
import           Data.Graph
import qualified Data.Set as Set
import           Data.Text (Text)
import           Inflex.Types
import           Inflex.Types.Renamer

--------------------------------------------------------------------------------
-- Types

data LoadError
  = CycleError [Text]
  | RenameLoadError ParseRenameError
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level entry points

toplogicalSort ::
     [Named (Either ParseRenameError (IsRenamed a))]
  -> [Named (Either LoadError (IsRenamed a))]
toplogicalSort = concatMap cycleCheck . stronglyConnCompR . map toNode
  where
    toNode named@Named {name, thing = result} =
      case result of
        Right IsRenamed {unresolvedGlobals} ->
          (named, name, Set.toList unresolvedGlobals)
        Left {} -> (named, name, mempty)
    cycleCheck =
      \case
        AcyclicSCC (named, _, _) -> [fmap (first RenameLoadError) named]
        CyclicSCC nameds ->
          fmap
            (\(named, _, _) ->
               fmap
                 (const (Left (CycleError (map (\(_, name, _) -> name) nameds))))
                 named)
            nameds
