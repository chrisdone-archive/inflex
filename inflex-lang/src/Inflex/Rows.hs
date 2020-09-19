{-# LANGUAGE RecordWildCards #-}
-- | Functions pertaining to row types.

module Inflex.Rows where

import Inflex.Types

-- | Make a polymorphic row type.
polymorphicTypeRow :: StagedLocation s -> TypeVariable s -> [Field s] -> TypeRow s
polymorphicTypeRow location var fs = TypeRow {typeVariable = Just var, fields = fs, ..}

-- | Make a monomorphic row type.
monomorphicTypeRow :: StagedLocation s -> [Field s] -> TypeRow s
monomorphicTypeRow location fs = TypeRow {typeVariable = Nothing, fields = fs, ..}
