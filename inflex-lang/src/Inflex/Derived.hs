{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions easily derived from primitive functions.

module Inflex.Derived where

import Inflex.Derived.TH
import Inflex.Types

nullFunction :: Expression Resolved
nullFunction = $(compile "list: @prim:array_length(list) = (0::Integer)")

from_okFunction :: Expression Resolved
from_okFunction = $(compile "def: v: case v { #ok(a): a, o: def }")

anyFunction :: Expression Resolved
anyFunction = $(compile "pred: list: case list.@prim:array_find(pred) { #ok(e): #true, e: #false }")
