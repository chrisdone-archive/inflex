{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions easily derived from primitive functions.

module Inflex.Derived where

import Inflex.Derived.TH
import Inflex.Types

nullFunction :: Expression Resolved
nullFunction = $(compile "list: length(list) = (0::Integer)")
