-- | Functions pertaining to row types.

module Inflex.Rows where

import Inflex.Types

-- | Make a polymorphic row type.
polymorphicTypeRow :: TypeVariable s -> [Field s] -> TypeRow s
polymorphicTypeRow var fs = TypeRow {typeVariable = Just var, fields = fs}

-- | Make a monomorphic row type.
monomorphicTypeRow :: [Field s] -> TypeRow s
monomorphicTypeRow fs = TypeRow {typeVariable = Nothing, fields = fs}
