{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Inflex.Variants where

import Inflex.Type
import Inflex.Types

--------------------------------------------------------------------------------
-- Bool

reifyBool :: Expression Resolved -> Maybe Bool
reifyBool =
  \case
    VariantExpression Variant {tag = TagName "true", argument = Nothing} ->
      Just True
    VariantExpression Variant {tag = TagName "false", argument = Nothing} ->
      Just False
    _ -> Nothing

trueVariant :: StagedLocation Resolved -> Expression Resolved
trueVariant location =
  VariantExpression
    Variant {location, typ = boolType location, tag = TagName "true", argument = Nothing}

falseVariant :: StagedLocation Resolved -> Expression Resolved
falseVariant location =
  VariantExpression
    Variant {location, typ = boolType location, tag = TagName "false", argument = Nothing}

--------------------------------------------------------------------------------
-- Ordering

equalVariant :: StagedLocation Resolved -> Expression Resolved
equalVariant location =
  VariantExpression
    Variant {location, typ = boolType location, tag = TagName "=", argument = Nothing}

lessVariant :: StagedLocation Resolved -> Expression Resolved
lessVariant location =
  VariantExpression
    Variant {location, typ = boolType location, tag = TagName "<", argument = Nothing}

moreVariant :: StagedLocation Resolved -> Expression Resolved
moreVariant location =
  VariantExpression
    Variant {location, typ = boolType location, tag = TagName ">", argument = Nothing}
