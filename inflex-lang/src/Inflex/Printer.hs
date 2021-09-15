{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The printer of code.

module Inflex.Printer where

import qualified Data.ByteString.Lazy as L
import           Data.Aeson (encode)
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isAlphaNum)
import           Data.Coerce
import           Data.Foldable
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           Inflex.Instances ()
import           Inflex.Location
import           Inflex.Types
import           Inflex.Types.SHA512
import qualified RIO

{-# INLINE tracePrinter #-}
tracePrinter :: Printer a1 => a1 -> a2 -> a2
tracePrinter e = trace (L8.unpack (SB.toLazyByteString (RIO.getUtf8Builder (printer e))))

printerText :: Printer a => a -> Text
printerText =
  T.decodeUtf8 . L.toStrict . SB.toLazyByteString . RIO.getUtf8Builder . printer

class Printer a where
  printer :: a -> RIO.Utf8Builder

instance Printer Text where
  printer = RIO.display

instance Printer Integer where
  printer = RIO.display

-- TODO: Handle scope and retain original names, if provided.
-- TODO: Avoid unneeded parens.

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
      BoundaryExpression {} -> error "boundary"
      EarlyExpression {} -> error "early"
      InfixExpression infix' -> printer infix'

instance Printer (Case Resolved) where
  printer Case{..} = "case " <> printer scrutinee <> " {" <>
    mconcat (intersperse ", " (map printer (toList alternatives)))
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
    "[" <> mconcat (intersperse ", " (map printer (toList expressions))) <> "]"

instance Printer (Variant Resolved) where
  printer (Variant {tag, argument}) =
    printer tag <> (if not (null argument)
                       then "(" <> mconcat (intersperse ", " (map printer (toList argument))) <> ")"
                       else mempty)

instance Printer TagName where
  printer (TagName s) ="#" <> printer s

instance Printer (Record Resolved) where
  printer (Record {fields}) =
    "{" <>
    mconcat
      (intersperse
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
               TextLiteral LiteralText{text} -> printText text

instance Printer (Number Resolved) where
  printer (Number {number}) = printer number

instance Printer (Lambda Resolved) where
  printer Lambda{location,body} =
    case location of
      ImplicitArgumentFor {} -> printer body
      _ -> ":" <> printer body

instance Printer (Variable Resolved) where
  printer Variable{name} = "$" <> RIO.displayShow (coerce (deBrujinIndexNesting name) :: Int)

instance Printer (Global Resolved) where
  printer Global{name} = printer name

instance Printer (GlobalRef Resolved) where
  printer = \case
               HashGlobal (Hash hash) -> "#" <> RIO.displayShow hash
               FromIntegerGlobal -> "@prim:from_integer"
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
               FunctionGlobal function -> "@prim:" <> printer function

instance Printer (Apply Resolved) where
  printer apply@Apply {function, argument, style} =
    case style of
      OverloadedApply -> printer argument
      _ ->
        case expressionLocation argument of
          ImplicitArgumentFor {} -> printer function
          AutoInsertedForDefaulterCursor {} -> printer function
             -- TODO: Hides the implicit function applications generated by the
             -- renamer. This isn't very clean. But it requires more thought to
             -- handle this.
          _ ->
            case apply of
              Apply { function = GlobalExpression Global {name = FromDecimalGlobal}
                    , argument = LiteralExpression {}
                    } -> printer argument
              Apply { function = GlobalExpression Global {name = FromIntegerGlobal}
                    , argument = LiteralExpression {}
                    } -> printer argument
              _ -> printerApplyResolved apply

--------------------------------------------------------------------------------
-- Renamed

instance Printer (Variant Renamed) where
  printer (Variant {tag, argument}) =
    printer tag <>
    (if not (null argument)
       then "(" <> mconcat (intersperse ", " (map printer (toList argument))) <>
            ")"
       else mempty)

instance Printer (Expression Renamed) where
  printer =
    \case
      RecordExpression record -> printer record
      BoundaryExpression boundary' -> printer boundary'
      EarlyExpression early' -> printer early'
      IfExpression if' -> printer if'
      CaseExpression case' -> printer case'
      VariantExpression variant -> printer variant
      PropExpression prop -> printer prop
      HoleExpression hole -> printer hole
      ArrayExpression array -> printer array
      LiteralExpression literal -> printer literal
      LambdaExpression lambda -> printer lambda
      ApplyExpression apply -> printer apply
      VariableExpression variable -> printer variable
      GlobalExpression global -> printer global
      LetExpression let' -> printer let'
      InfixExpression infix' -> printer infix'

instance Printer (Early Renamed) where
  printer Early{..} = printer expression <> "?"

instance Printer (Boundary Renamed) where
  printer Boundary{..} = "early { " <> printer expression <> " }"

instance Printer (Case Renamed) where
  printer Case{..} = "case " <> printer scrutinee <> " {" <>
    mconcat (intersperse ", " (map printer (toList alternatives)))
    <> "}"

instance Printer (If Renamed) where
  printer If {..} =
    "if " <> printer condition <> " then " <> printer consequent <> " else " <>
    printer alternative

instance Printer (Alternative Renamed) where
  printer Alternative {..} =
    case pattern' of
      ParamPattern _param ->
        "_" -- TODO: need to resolve from mappings.
         <>
        ": " <>
        printer expression
      VariantPattern variant -> printer variant <> ": " <> printer expression

instance Printer (VariantP Renamed) where
  printer VariantP {..} =
    printer tag <>
    (case argument of
       Nothing -> mempty
       Just _param -> "(_)") -- TODO: need to resolve from mappings.

instance Printer (Hole Renamed) where
  printer (Hole{}) = "_"

instance Printer (Prop Renamed) where
  printer (Prop {expression, name}) =
    printer expression <> "." <> printer name -- TODO: Manage parens.

instance Printer (Array Renamed) where
  printer (Array {expressions}) =
    "[" <> mconcat (intersperse ", " (map printer (toList expressions))) <> "]"

instance Printer (Record Renamed) where
  printer (Record {fields}) =
    "{" <>
    mconcat
      (intersperse
         ", "
         (map
            (\FieldE {name, expression} ->
               printer name <> ": " <> printer expression)
            fields)) <>
    "}"

instance Printer (Infix Renamed) where
  printer (Infix {left, global, right}) =
    "(" <> printer left <> " " <> printer global <> " " <> printer right <> ")"

instance Printer (Let Renamed) where
  printer (Let {body}) = "let ... in " <> printer body

instance Printer (Literal Renamed) where
  printer = \case
               NumberLiteral number -> printer number
               TextLiteral LiteralText{text} -> printText text

printText :: Text -> RIO.Utf8Builder
printText t =
  RIO.displayBytesUtf8 (T.encodeUtf8 ("\"" <> T.replace "\"" "\"\"" t <> "\""))

instance Printer (Number Renamed) where
  printer (Number {number}) = printer number

instance Printer (Lambda Renamed) where
  printer Lambda{param=_,body} = "(:" <> printer body <> ")"

instance Printer (Variable Renamed) where
  printer Variable{name} = "$" <> RIO.displayShow (coerce (deBrujinIndexNesting name) :: Int)

instance Printer (Global Renamed) where
  printer Global{name} = printer name

instance Printer (GlobalRef Renamed) where
  printer = \case
               HashGlobal (Hash hash) -> "#" <> RIO.displayShow hash
               FromIntegerGlobal -> "@prim:from_integer"
               FromDecimalGlobal -> "fromDecimal"
               CompareGlobal compareity -> case compareity of
                                         LessThan -> "<"
                                         GreaterThan -> ">"
                                         GreaterEqualTo -> ">="
                                         LessEqualTo -> "<="
               EqualGlobal equality -> case equality of
                                         Equal -> "="
                                         NotEqual -> "/="
               NumericBinOpGlobal op -> printer op
               FunctionGlobal function -> "@prim:" <> printer function

instance Printer InstanceName where
  printer =
    \case
      EqualIntegerInstance -> "<Equal Integer>"
      EqualTextInstance -> "<Equal Text>"
      EqualDecimalInstance n -> "<Equal (Decimal " <> RIO.displayShow n <> ")>"
      CompareIntegerInstance -> "<Compare Integer>"
      CompareTextInstance -> "<Compare Text>"
      CompareDecimalInstance n -> "<Compare (Decimal " <> RIO.displayShow n <> ")>"
      FromIntegerIntegerInstance -> "<FromInteger Integer>"
      FromIntegerDecimalInstance {} -> "<FromInteger Decimal>"
      FromDecimalDecimalInstance FromDecimalInstance { supersetPlaces
                                                     , subsetPlaces
                                                     } ->
        "<FromDecimal " <> RIO.displayShow supersetPlaces <> " (Decimal " <>
        RIO.displayShow subsetPlaces <>
        ")>"
      IntegerOpInstance op -> "<(" <> printer op <> ") @ Integer>"
      DecimalOpInstance nat op ->
        "<(" <> printer op <> ") @(Decimal " <>
        printer (fromIntegral nat :: Integer) <>
        ")>"

-- TODO: Hides the implicit function applications generated by the
-- renamer. This isn't very clean. But it requires more thought to
-- handle this.
instance Printer (Apply Renamed) where
  printer apply@Apply {location, argument} =
    case location of
      AutogeneratedCursor -> printer argument
      _ ->
        case apply of
          Apply { function = GlobalExpression Global {name = ExactGlobalRef FromIntegerGlobal}
                , argument = LiteralExpression {}
                } -> printer argument
          Apply { function = GlobalExpression Global {name = ExactGlobalRef FromDecimalGlobal}
                , argument = LiteralExpression {}
                } -> printer argument
          _ -> printerApply printer apply

--------------------------------------------------------------------------------
-- Parsed

instance Printer (Variant Parsed) where
  printer (Variant {tag, argument}) =
    printer tag <> (if not (null argument)
                       then "(" <> mconcat (intersperse ", " (map printer (toList argument))) <> ")"
                       else mempty)

instance Printer (Expression Parsed) where
  printer =
    \case
      RecordExpression record -> printer record
      BoundaryExpression boundary' -> printer boundary'
      EarlyExpression early' -> printer early'
      IfExpression if' -> printer if'
      CaseExpression case' -> printer case'
      VariantExpression variant -> printer variant
      PropExpression prop -> printer prop
      HoleExpression hole -> printer hole
      ArrayExpression array -> printer array
      LiteralExpression literal -> printer literal
      LambdaExpression lambda -> printer lambda
      ApplyExpression apply -> printer apply
      VariableExpression variable -> printer variable
      GlobalExpression global -> printer global
      LetExpression let' -> printer let'
      InfixExpression infix' -> printer infix'

instance Printer (Early Parsed) where
  printer Early{..} = printer expression <> "?"

instance Printer (Boundary Parsed) where
  printer Boundary{..} = "early { " <> printer expression <> " }"

instance Printer (Case Parsed) where
  printer Case{..} = "case " <> printer scrutinee <> " {" <>
    mconcat (intersperse ", " (map printer (toList alternatives)))
    <> "}"

instance Printer (If Parsed) where
  printer If {..} =
    "if " <> printer condition <> " then " <> printer consequent <> " else " <>
    printer alternative

instance Printer (Alternative Parsed) where
  printer Alternative {..} =
    case pattern' of
      ParamPattern param ->
        printer param
         <>
        ": " <>
        printer expression
      VariantPattern variant -> printer variant <> ": " <> printer expression

instance Printer (VariantP Parsed) where
  printer VariantP {..} =
    printer tag <>
    (case argument of
       Nothing -> mempty
       Just param -> "(" <> printer param <> ")")

instance Printer (Hole Parsed) where
  printer (Hole{}) = "_"

instance Printer (Prop Parsed) where
  printer (Prop {expression, name}) =
    printer expression <> "." <> printer name -- TODO: Manage parens.

instance Printer (Array Parsed) where
  printer (Array {expressions, typ}) =
    addColumnsIfNeeded expressions typ ("[" <> mconcat (intersperse ", " (map printer (toList expressions))) <> "]")

addColumnsIfNeeded :: Printer a => Vector e -> Maybe a -> RIO.Utf8Builder -> RIO.Utf8Builder
addColumnsIfNeeded expressions typ inner =
  case typ of
    Just t | V.null expressions -> inner <> " :: " <> printer t
    Just t -> inner <> " :: " <> printer t
    _ -> inner

-- TODO: Re-think this printer?
instance Printer (Type Parsed) where
  printer =
    \case
      ArrayType t -> "[" <> printer t <> "]"
      RecordType (RowType (TypeRow {fields})) ->
        "{" <>
        mconcat
          (intersperse
             ", "
             (map
                (\Field {name, typ} -> printer name <> ":" <> printer typ)
                fields)) <>
        "}"
      VariantType (RowType (TypeRow {fields})) ->
        "<" <>
        mconcat
          (intersperse
             ", "
             (map
                (\Field {name, typ} -> printer name <> ":" <> printer typ)
                fields)) <>
        "|_>" -- This is correct; a parsed type can't include variables at the moment.
      FreshType {} -> "_"
      ConstantType TypeConstant {name = IntegerTypeName} ->
        "Integer" -- TODO: change to @prim:integer-type)
      ConstantType TypeConstant {name = TextTypeName} ->
        "Text" -- TODO: change to @prim:text-type)
      ApplyType TypeApplication { function = ConstantType TypeConstant {name = DecimalTypeName}
                                , argument = ConstantType TypeConstant {name = NatTypeName n}
                                } -> "Decimal " <> RIO.displayShow n
      _ -> "_"

instance Printer (Record Parsed) where
  printer (Record {fields}) =
    "{" <>
    mconcat
      (intersperse
         ", "
         (map
            (\FieldE {name, expression} ->
               printer name <> ": " <> printer expression)
            fields)) <>
    "}"

-- TODO: Make much more robust.
instance Printer FieldName where
  printer (FieldName t) =
    if True -- Applying this for graph support. TODO: remove it.
            || T.any (not . printableNameChar) t
      then RIO.displayBytesUtf8 (L.toStrict (encode t))
      else printer t

printableNameChar :: Char -> Bool
printableNameChar '_' = True
printableNameChar c = isAlphaNum c

instance Printer (Infix Parsed) where
  printer (Infix {left, global, right}) =
    "(" <> printer left <> " " <> printer global <> " " <> printer right <> ")"

instance Printer (Let Parsed) where
  printer (Let {body}) = "let ... in " <> printer body

instance Printer (Literal Parsed) where
  printer = \case
               NumberLiteral number -> printer number
               TextLiteral LiteralText{text} -> printText text

instance Printer (Number Parsed) where
  printer (Number {number}) = printer number

instance Printer SomeNumber where
  printer = \case
               IntegerNumber i -> printer i
               DecimalNumber decimal -> RIO.display decimal

instance Printer (Lambda Parsed) where
  printer Lambda{param,body} = "(" <> printer param <> ":" <> printer body <> ")"

instance Printer (Param Parsed) where
  printer Param{name} = printer name

instance Printer (Variable Parsed) where
  printer Variable{name} = printer name

instance Printer (Global Parsed) where
  printer Global{name} = printer name

instance Printer IncompleteGlobalRef where
  printer =
    \case
      UnresolvedGlobalText text -> printer text
      UnresolvedUuid (Uuid uuid) -> "@uuid:" <> printer uuid
      ExactGlobalRef ref -> printer ref
      ResolvedGlobalRef text _ -> printer text

instance Printer ParsedGlobal where
  printer = \case
               ParsedTextName name -> printer name
               ParsedHash (Hash hash) -> "#" <> printer (sha512AsHexText hash)
               ParsedUuid (Uuid uuid) -> "@uuid:" <> printer uuid
               ParsedPrim fun -> "@prim:" <> printer fun
               ParsedFromInteger -> "@prim:from_integer"
               ParsedFromDecimal -> "@prim:from_decimal"

instance Printer (GlobalRef Parsed) where
  printer =
    \case
      HashGlobal (Hash hash) -> "#" <> RIO.displayShow hash
      FromIntegerGlobal -> "@prim:from_integer"
      FromDecimalGlobal -> "@prim:from_decimal"
      CompareGlobal compareity ->
        case compareity of
          LessThan -> "<"
          GreaterThan -> ">"
          GreaterEqualTo -> ">="
          LessEqualTo -> "<="
      EqualGlobal equality ->
        case equality of
          Equal -> "="
          NotEqual -> "/="
      NumericBinOpGlobal op -> printer op
      FunctionGlobal function -> "@prim:" <> printer function

instance Printer Function where
  printer =
    \case
      MapFunction -> "array_map"
      FromOkFunction -> "from_ok"
      VegaFunction -> "vega"
      NotFunction -> "not"
      FilterFunction -> "array_filter"
      DistinctFunction -> "array_distinct"
      SortFunction -> "array_sort"
      ConcatFunction -> "array_concat"
      AndFunction -> "array_and"
      OrFunction -> "array_or"
      SumFunction -> "array_sum"
      MinimumFunction -> "array_minimum"
      MaximumFunction -> "array_maximum"
      AverageFunction -> "array_average"
      LengthFunction -> "array_length"
      FindFunction -> "array_find"
      AllFunction -> "array_all"
      AnyFunction -> "array_any"
      NullFunction -> "array_null"

instance Printer NumericBinOp where
  printer =
    \case
      MulitplyOp -> "*"
      AddOp -> "+"
      SubtractOp -> "-"
      DivideOp -> "/"

instance Printer (Apply Parsed) where
  printer  = printerApply printer

printerApply :: (Expression s -> RIO.Utf8Builder) -> Apply s -> RIO.Utf8Builder
printerApply printer' apply =
  printer' function <> "(" <>
  mconcat (intersperse ", " (map printer' arguments)) <>
  ")"
  where (function, arguments) = uncurryApplies apply

printerApplyResolved :: Apply Resolved -> RIO.Utf8Builder
printerApplyResolved apply =
  printer function <> "(" <>
  mconcat (intersperse ", " (map printer arguments)) <>
  ")"
  where (function, arguments0) = uncurryApplies apply
        arguments = filter (\e -> case expressionLocation e of
                                    ImplicitArgumentFor{} -> False
                                    _ -> True) arguments0

uncurryApplies :: Apply s -> (Expression s, [Expression s])
uncurryApplies Apply {function, argument} =
  case function of
    ApplyExpression apply ->
      let !(!actualFunction, !arguments) = uncurryApplies apply
      in (actualFunction, arguments <> [argument])
    actualFunction -> (actualFunction, [argument])
