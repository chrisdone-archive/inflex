{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Variants where

import Inflex.Type
import Inflex.Types

trueVariant :: StagedLocation Resolved -> Expression Resolved
trueVariant location =
  VariantExpression
    Variant {location, typ = boolType location, tag = TagName "true", argument = Nothing}

falseVariant :: StagedLocation Resolved -> Expression Resolved
falseVariant location =
  VariantExpression
    Variant {location, typ = boolType location, tag = TagName "false", argument = Nothing}
