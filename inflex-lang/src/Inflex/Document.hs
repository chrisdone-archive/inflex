{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Loading a document of cells.

module Inflex.Document where

import           Control.Parallel.Strategies
import           Data.Bifunctor
import           Data.Graph
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Inflex.Renamer
import           Inflex.Types

--------------------------------------------------------------------------------
-- Types

data LoadError
  = CycleError [Text]
  | RenameLoadError ParseRenameError
  | DuplicateName
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level entry points

-- | Lex, parse, rename -- can all be done per cell in parallel.
independentLoad ::
     [Named Text] -> [Named (Either LoadError (IsRenamed (Expression Renamed)))]
independentLoad names =
  parMap
    rseq
    (\Named {..} ->
       Named
         { thing =
             if any
                  (\Named {uuid = uuid', name = name'} ->
                     uuid' /= uuid && name == name')
                  names -- TODO: Fix this O(n^2) operation
               then Left DuplicateName
               else first RenameLoadError (renameText (T.unpack name) thing)
         , ..
         })
    names

--------------------------------------------------------------------------------
-- Internal work

-- | Sort the named cells in the document by reverse dependency order.
topologicalSort ::
     [Named (Either LoadError (IsRenamed a))]
  -> [Named (Either LoadError (IsRenamed a))]
topologicalSort = concatMap cycleCheck . stronglyConnCompR . map toNode
  where
    toNode named@Named {name, thing = result} =
      case result of
        Right IsRenamed {unresolvedGlobals} ->
          (named, name, Set.toList unresolvedGlobals)
        Left {} -> (named, name, mempty)
    cycleCheck =
      \case
        AcyclicSCC (named, _, _) -> [named]
        CyclicSCC nameds ->
          fmap
            (\(named, _, _) ->
               fmap
                 (const (Left (CycleError (map (\(_, name, _) -> name) nameds))))
                 named)
            nameds
