{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Printer for developers of Inflex / debugging.

module Inflex.Printer
  ( printer
  , tracePrinter
  ) where

import           Data.Aeson (encode)
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isAlphaNum)
import           Data.Coerce
import           Data.Foldable
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Debug.Trace
import           Inflex.Decimal
import           Inflex.Types
import qualified RIO
import           RIO (displayBytesUtf8, displayShow, Utf8Builder)

{-# INLINE tracePrinter #-}
tracePrinter :: Printer a1 => a1 -> a2 -> a2
tracePrinter e = trace (L8.unpack (SB.toLazyByteString (RIO.getUtf8Builder (printer e))))

class Printer a where
  printer :: a -> Utf8Builder

instance Printer Text where
  printer = RIO.display

instance Printer Integer where
  printer = RIO.display

instance Printer (Expression Resolved) where
  printer =
    \case
      RecordExpression record -> printer record
      PropExpression prop -> printer prop
      HoleExpression hole -> printer hole
      ArrayExpression array -> printer array
      VariantExpression variant -> printer variant
      LiteralExpression literal -> printer literal
      LambdaExpression lambda -> printer lambda
      ApplyExpression apply -> printer apply
      VariableExpression variable -> printer variable
      GlobalExpression global -> printer global
      LetExpression let' -> printer let'
      IfExpression if' -> printer if'
      CaseExpression case' -> printer case'
      EarlyExpression early' -> printer early'
      BoundaryExpression boundary' -> printer boundary'
      InfixExpression infix' -> printer infix'

instance Printer (Case Resolved) where
  printer Case{..} = "case " <> printer scrutinee <> " {" <>
    mconcat (List.intersperse ", " (map printer (toList alternatives)))
    <> "}"

instance Printer (Early Resolved) where
  printer Early{..} = printer expression <> "?"

instance Printer (Boundary Resolved) where
  printer Boundary{..} = "early { " <> printer expression <> " }"

instance Printer (If Resolved) where
  printer If {..} =
    "if " <> printer condition <> " then " <> printer consequent <> " else " <>
    printer alternative

instance Printer (Alternative Resolved) where
  printer Alternative {..} =
    case pattern' of
      ParamPattern _param ->
        "_" -- TODO: need to resolve from mappings.
         <>
        ": " <>
        printer expression
      VariantPattern variant -> printer variant <> ": " <> printer expression

instance Printer (VariantP Resolved) where
  printer VariantP {..} =
    printer tag <>
    (case argument of
       Nothing -> mempty
       Just _param -> "(_)") -- TODO: need to resolve from mappings.

instance Printer (Hole Resolved) where
  printer (Hole{}) = "_"

instance Printer (Prop Resolved) where
  printer (Prop {expression, name}) =
    printer expression <> "." <> printer name -- TODO: Manage parens.

instance Printer (Array Resolved) where
  printer (Array {expressions}) =
    "[" <> mconcat (List.intersperse ", " (map printer (toList expressions))) <> "]"

instance Printer (Variant Resolved) where
  printer (Variant {tag, argument}) =
    printer tag <> (if not (null argument)
                       then "(" <> mconcat (List.intersperse ", " (map printer (toList argument))) <> ")"
                       else mempty)

instance Printer TagName where
  printer (TagName s) ="#" <> printer s

instance Printer (Record Resolved) where
  printer (Record {fields}) =
    "{" <>
    mconcat
      (List.intersperse
         ", "
         (map
            (\FieldE {name, expression} ->
               printer name <> ": " <> printer expression)
            fields)) <>
    "}"

instance Printer (Infix Resolved) where
  printer (Infix {left, global, right}) =
    "(" <> printer left <> " " <> printer global <> " " <> printer right <> ")"

instance Printer (Let Resolved) where
  printer (Let {body}) = "let ... in " <> printer body

instance Printer (Literal Resolved) where
  printer = \case
               NumberLiteral number -> printer number
               TextLiteral LiteralText{text} -> displayText text

displayText :: Text -> Utf8Builder
displayText t =
  displayBytesUtf8 (T.encodeUtf8 ("\"" <> T.replace "\"" "\"\"" t <> "\""))

instance Printer (Number Resolved) where
  printer (Number {number}) = printer number

instance Printer (Lambda Resolved) where
  printer Lambda{body} =
    "\\ -> " <> printer body

instance Printer (Variable Resolved) where
  printer Variable{name} = "$" <> displayShow (coerce (deBrujinIndexNesting name) :: Int)

instance Printer (Global Resolved) where
  printer Global{name} = printer name

instance Printer (GlobalRef Resolved) where
  printer = \case
               HashGlobal (Hash hash) -> "#" <> displayShow hash
               FromIntegerGlobal -> "fromInteger"
               FromDecimalGlobal -> "fromDecimal"
               EqualGlobal equality -> case equality of
                                         Equal -> "="
                                         NotEqual -> "/="
               CompareGlobal compareity -> case compareity of
                                         LessThan -> "<"
                                         GreaterThan -> ">"
                                         GreaterEqualTo -> ">="
                                         LessEqualTo -> "="
               NumericBinOpGlobal op -> printer op
               InstanceGlobal r -> printer r
               FunctionGlobal function -> printer function

instance Printer (Apply Resolved) where
  printer Apply {function, argument} =
    mconcat ["(", printer function,")","(", printer argument,")"]

instance Printer FieldName where
  printer (FieldName t) =
    if True -- Applying this for graph support. TODO: remove it.
            || T.any (not . printableNameChar) t
      then displayBytesUtf8 (L.toStrict (encode t))
      else printer t

printableNameChar :: Char -> Bool
printableNameChar '_' = True
printableNameChar c = isAlphaNum c

instance Printer SomeNumber where
  printer = \case
               IntegerNumber i -> printer i
               DecimalNumber decimal -> printer decimal

instance Printer Decimal where
  printer decimal =
    case decimalToFixed decimal of
      SomeFixed _ f -> displayShow f

instance Printer NumericBinOp where
  printer =
    \case
      MulitplyOp -> "*"
      AddOp -> "+"
      SubtractOp -> "-"
      DivideOp -> "/"

instance Printer InstanceName where
  printer =
    \case
      EqualIntegerInstance -> "<Equal Integer>"
      EqualTextInstance -> "<Equal Text>"
      EqualDecimalInstance n -> "<Equal (Decimal " <> displayShow n <> ")>"
      CompareIntegerInstance -> "<Compare Integer>"
      CompareTextInstance -> "<Compare Text>"
      CompareDecimalInstance n -> "<Compare (Decimal " <> displayShow n <> ")>"
      FromIntegerIntegerInstance -> "<FromInteger Integer>"
      FromIntegerDecimalInstance {} -> "<FromInteger Decimal>"
      FromDecimalDecimalInstance FromDecimalInstance { supersetPlaces
                                                     , subsetPlaces
                                                     } ->
        "<FromDecimal " <> displayShow supersetPlaces <> " (Decimal " <>
        displayShow subsetPlaces <>
        ")>"
      IntegerOpInstance op -> "<" <> displayShow op <> " Integer>"
      DecimalOpInstance nat op ->
        "<" <> displayShow op <> " Decimal " <>
        printer (fromIntegral nat :: Integer) <>
        ">"

instance Printer Function where
  printer = \case
    MapFunction -> "map"
    FromOkFunction -> "from_ok"
    VegaFunction -> "vega"
    FilterFunction -> "filter"
    DistinctFunction -> "distinct"
    SortFunction -> "sort"
    ConcatFunction -> "concat"
    AndFunction -> "and"
    OrFunction -> "or"
    SumFunction -> "sum"
    MinimumFunction -> "minimum"
    MaximumFunction -> "maximum"
    AverageFunction -> "average"
    LengthFunction -> "length"
    FindFunction -> "find"
    AllFunction -> "all"
    AnyFunction -> "any"
    NullFunction -> "null"
