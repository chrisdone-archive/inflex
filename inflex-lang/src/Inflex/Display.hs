{-# LANGUAGE GADTs #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pretty printer for debugging.

module Inflex.Display where

import Data.Coerce
import Inflex.Decimal
import Inflex.Types
import RIO

instance Display (Expression Resolved) where
  display =
    \case
      LiteralExpression literal -> display literal
      LambdaExpression lambda -> display lambda
      ApplyExpression apply -> display apply
      VariableExpression variable -> display variable
      GlobalExpression global -> display global
      LetExpression let' -> display let'
      InfixExpression infix' -> display infix'

instance Display (Infix Resolved) where
  display (Infix {left, global, right}) =
    "(" <> display left <> " " <> display global <> " " <> display right <> ")"

instance Display (Let Resolved) where
  display (Let {body}) = "let ... in " <> display body

instance Display (Literal Resolved) where
  display = \case
               NumberLiteral number -> display number

instance Display (Number Resolved) where
  display (Number {number}) = display number

instance Display SomeNumber where
  display = \case
               IntegerNumber i -> display i
               DecimalNumber decimal -> display decimal

instance Display Decimal where
  display decimal =
    case decimalToFixed decimal of
      SomeFixed _ f -> displayShow f

instance Display (Lambda Resolved) where
  display Lambda{body} = "(\\ -> " <> display body <> ")"

instance Display (Variable Resolved) where
  display Variable{name} = "$" <> displayShow (coerce (deBrujinIndexNesting name) :: Int)

instance Display (Global Resolved) where
  display Global{name} = display name

instance Display (GlobalRef Resolved) where
  display = \case
               HashGlobal (Hash hash) -> "#" <> displayShow hash
               FromIntegerGlobal -> "fromInteger"
               FromDecimalGlobal -> "fromDecimal"
               NumericBinOpGlobal op -> display op
               InstanceGlobal r -> display r

instance Display NumericBinOp where
  display =
    \case
      MulitplyOp -> "*"
      AddOp -> "+"
      SubtractOp -> "-"
      DivideOp -> "/"

instance Display InstanceName where
  display =
    \case
      FromIntegerIntegerInstance -> "<FromInteger Integer>"
      FromIntegerDecimalInstance {} -> "<FromInteger Decimal>"
      FromDecimalDecimalInstance {} -> "<FromDecimal Decimal"
      IntegerOpInstance op -> "<" <> display op <> " Integer>"
      DecimalOpInstance nat op ->
        "<" <> display op <> " Decimal " <>
        display (fromIntegral nat :: Integer) <>
        ">"

instance Display (Apply Resolved) where
  display Apply {function, argument} =
    "(" <> display function <> " " <> display argument <> ")"
