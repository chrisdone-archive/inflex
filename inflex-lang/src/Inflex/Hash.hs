{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Hashing.

module Inflex.Hash where

import Inflex.Instances ()
import Inflex.Resolver
import Inflex.Types
import Inflex.Types.SHA512

-- TODO: Swap the use of show for something more structured (and faster).
hashExpression :: (Show (Expression s)) => Expression s -> Hash
hashExpression = Hash . sha512String . show

hashResolved :: IsResolved (Expression Resolved) -> Hash
hashResolved IsResolved {thing} = hashExpression thing

hashCell :: Cell -> Hash
hashCell Cell {defaulted} = hashExpression defaulted
