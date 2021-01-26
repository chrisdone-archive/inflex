{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
-- |

module Inflex.Pretty where

import Data.Char
import Inflex.Types

prettyEquality :: EqualityConstraint -> String
prettyEquality EqualityConstraint {type1,type2} =
  prettyType type1 <> " ~ " <> prettyType type2

prettyType :: Type Generated -> String
prettyType =
  \case
    VariableType TypeVariable {prefix, index} -> map toLower (show prefix) ++ show index
    ArrayType t -> "[" ++ prettyType t ++ "]"

    ApplyType TypeApplication { function = ApplyType TypeApplication { function = ConstantType (TypeConstant {name = FunctionTypeName})
                                                                     , argument = i
                                                                     }
                              , argument = o
                              } ->
      "(" ++ prettyType i ++ " -> " ++ prettyType o ++ ")"
    ApplyType TypeApplication {function = f, argument = x} ->
      "(" ++ prettyType f ++ " " ++ prettyType x ++ ")"
    ConstantType TypeConstant {name} -> show name
    _ -> "?"
