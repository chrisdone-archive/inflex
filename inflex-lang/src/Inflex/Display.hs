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

import           Data.Coerce
import           Data.List
import qualified Data.Text as T
import           Inflex.Decimal
import           Inflex.Types
import           RIO

-- TODO: Avoid unneeded parens.

instance Display (Expression Resolved) where
  display =
    \case
      RecordExpression record -> display record
      PropExpression prop -> display prop
      ArrayExpression array -> display array
      LiteralExpression literal -> display literal
      LambdaExpression lambda -> display lambda
      ApplyExpression apply -> display apply
      VariableExpression variable -> display variable
      GlobalExpression global -> display global
      LetExpression let' -> display let'
      InfixExpression infix' -> display infix'

instance Display (Prop Resolved) where
  display (Prop {expression, name}) =
    display expression <> "." <> display name -- TODO: Manage parens.

instance Display (Array Resolved) where
  display (Array {expressions}) =
    "[" <> mconcat (intersperse ", " (map display (toList expressions))) <> "]"

instance Display (Record Resolved) where
  display (Record {fields}) =
    "{" <>
    mconcat
      (intersperse
         ", "
         (map
            (\FieldE {name, expression} ->
               display name <> ": " <> display expression)
            fields)) <>
    "}"

instance Display (Infix Resolved) where
  display (Infix {left, global, right}) =
    "(" <> display left <> " " <> display global <> " " <> display right <> ")"

instance Display (Let Resolved) where
  display (Let {body}) = "let ... in " <> display body

instance Display (Literal Resolved) where
  display = \case
               NumberLiteral number -> display number
               TextLiteral LiteralText{text} -> display (T.pack (show text))

instance Display (Number Resolved) where
  display (Number {number}) = display number

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
               FunctionGlobal function -> display function

instance Display (Apply Resolved) where
  display Apply {function, argument} =
    "(" <> display function <> " " <> display argument <> ")"


--------------------------------------------------------------------------------
-- Renamed

instance Display (Expression Renamed) where
  display =
    \case
      RecordExpression record -> display record
      PropExpression prop -> display prop
      ArrayExpression array -> display array
      LiteralExpression literal -> display literal
      LambdaExpression lambda -> display lambda
      ApplyExpression apply -> display apply
      VariableExpression variable -> display variable
      GlobalExpression global -> display global
      LetExpression let' -> display let'
      InfixExpression infix' -> display infix'

instance Display (Prop Renamed) where
  display (Prop {expression, name}) =
    display expression <> "." <> display name -- TODO: Manage parens.

instance Display (Array Renamed) where
  display (Array {expressions}) =
    "[" <> mconcat (intersperse ", " (map display (toList expressions))) <> "]"

instance Display (Record Renamed) where
  display (Record {fields}) =
    "{" <>
    mconcat
      (intersperse
         ", "
         (map
            (\FieldE {name, expression} ->
               display name <> ": " <> display expression)
            fields)) <>
    "}"

instance Display FieldName where
  display (FieldName t) = display t

instance Display (Infix Renamed) where
  display (Infix {left, global, right}) =
    "(" <> display left <> " " <> display global <> " " <> display right <> ")"

instance Display (Let Renamed) where
  display (Let {body}) = "let ... in " <> display body

instance Display (Literal Renamed) where
  display = \case
               NumberLiteral number -> display number
               TextLiteral LiteralText{text} -> display (T.pack (show text))

instance Display (Number Renamed) where
  display (Number {number}) = display number

instance Display SomeNumber where
  display = \case
               IntegerNumber i -> display i
               DecimalNumber decimal -> display decimal

instance Display Decimal where
  display decimal =
    case decimalToFixed decimal of
      SomeFixed _ f -> displayShow f

instance Display (Lambda Renamed) where
  display Lambda{body} = "(\\ -> " <> display body <> ")"

instance Display (Variable Renamed) where
  display Variable{name} = "$" <> displayShow (coerce (deBrujinIndexNesting name) :: Int)

instance Display (Global Renamed) where
  display Global{name} = display name

instance Display IncompleteGlobalRef where
  display =
    \case
      UnresolvedGlobal text -> display text
      GlobalRef ref -> display ref

instance Display (GlobalRef Renamed) where
  display = \case
               HashGlobal (Hash hash) -> "#" <> displayShow hash
               FromIntegerGlobal -> "fromInteger"
               FromDecimalGlobal -> "fromDecimal"
               NumericBinOpGlobal op -> display op
               FunctionGlobal function -> display function

instance Display Function where
  display = \case
    MapFunction -> "map"

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
      FromDecimalDecimalInstance FromDecimalInstance { supersetPlaces
                                                     , subsetPlaces
                                                     } ->
        "<FromDecimal " <> displayShow supersetPlaces <> " (Decimal " <>
        displayShow subsetPlaces <>
        ")>"
      IntegerOpInstance op -> "<(" <> display op <> ") @ Integer>"
      DecimalOpInstance nat op ->
        "<(" <> display op <> ") @(Decimal " <>
        display (fromIntegral nat :: Integer) <>
        ")>"

-- Hides the implicit function applications generated by the
-- renamer. This isn't very clean. But it requires more thought to
-- handle this.
instance Display (Apply Renamed) where
  display Apply {location, function, argument} =
    case location of
      AutogeneratedCursor -> display argument
      _ -> "(" <> display function <> " " <> display argument <> ")"
