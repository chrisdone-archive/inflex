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

reifyBool :: Expression Resolved -> Bool
reifyBool =
  \case
    VariantExpression Variant {tag = TagName "true", argument = Nothing} ->
      True
    VariantExpression Variant {tag = TagName "false", argument = Nothing} ->
      False
    _ -> error "reifyBool: Invalid bool!"

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
