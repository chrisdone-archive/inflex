{-# LANGUAGE RecordWildCards, TypeApplications, ScopedTypeVariables #-}
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

module Inflex.Printer
  ( tracePrinter
  , Printer
  , PrinterConfig(..)
  , emptyPrinterConfig
  , printer
  , printerText
  , runPrinter
  ) where

import           Control.Monad.Reader
import           Data.Aeson (encode)
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isAlphaNum)
import           Data.Coerce
import           Data.Foldable
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.String
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

emptyPrinterConfig :: PrinterConfig
emptyPrinterConfig = PrinterConfig {nameMappings = mempty}

{-# INLINE tracePrinter #-}
tracePrinter :: Printer a1 => PrinterConfig -> a1 -> a2 -> a2
tracePrinter cfg e =
  trace
    (L8.unpack
       (SB.toLazyByteString (RIO.getUtf8Builder (runPrinter cfg (printer e)))))

printerText :: Printer a => PrinterConfig -> a -> Text
printerText cfg =
  T.decodeUtf8 .
  L.toStrict .
  SB.toLazyByteString . RIO.getUtf8Builder . runPrinter cfg . printer

instance Semigroup Print where
  (<>) x y = Print ((<>) <$> runPrint x <*> runPrint y)

instance Monoid Print where
  mempty = Print (pure mempty)
  mappend = (<>)

instance IsString Print where
  fromString = Print . pure . fromString

data PrinterConfig = PrinterConfig
  { nameMappings :: Map Cursor Text
  -- TODO: Add whether parens are needed, then add two combinators:
  -- 1. A `parens' combinator that'll add parens if needed, where the default is true.
  -- 2. A `naked` combinator that'll say that parens aren't needed in certain cases.
  }

data Print = Print
  { runPrint :: Reader PrinterConfig RIO.Utf8Builder
  }

runPrinter :: PrinterConfig -> Print -> RIO.Utf8Builder
runPrinter cf (Print x) = runReader x cf

printShow :: Show a => a -> Print
printShow = Print . pure . RIO.displayShow

class Printer a where
  printer :: a -> Print

instance Printer Text where
  printer = Print . pure . RIO.display

instance Printer Integer where
  printer = Print . pure  . RIO.display

-- TODO: Handle scope and retain original names, if provided.
-- TODO: Avoid unneeded parens.

instance Stage s => Printer (Expression s) where
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
      CaseExpression case' -> printer case'
      InfixExpression infix' -> printer infix'

instance Stage s => Printer (Case s) where
  printer Case {..} =
    "if " <> printer scrutinee <> " {" <>
    (mconcat . intersperse ", ") (map printer (toList alternatives)) <>
    "}"

instance Stage s => Printer (Alternative s) where
  printer Alternative {..} =
    case reflectStage @s of
      StageResolved ->
        case pattern' of
          WildPattern {} -> "_: " <> printer expression
          ParamPattern param -> printer param <> ": " <> printer expression
          VariantPattern variant ->
            printer variant <> ": " <> printer expression
      StageParsed ->
        case pattern' of
          WildPattern {} -> "_: " <> printer expression
          ParamPattern param -> printer param <> ": " <> printer expression
          VariantPattern variant ->
            printer variant <> ": " <> printer expression

instance Stage s => Printer (VariantP s) where
  printer VariantP {..} =
    case reflectStage @s of
      StageResolved ->
        printer tag <>
        (case argument of
           Nothing -> mempty
           Just param -> "(" <> printer param <> ")"
         )
      StageParsed ->
        printer tag <>
        (case argument of
           Nothing -> mempty
           Just param -> "(" <> printer param <> ")")

instance Stage s =>  Printer (Hole s) where
  printer (Hole{}) = "_"

instance Stage s =>  Printer (Prop s) where
  printer (Prop {expression, name}) =
    printer expression <> "." <> printer name -- TODO: Manage parens.

instance Stage s => Printer (Array s) where
  printer (Array {expressions, typ}) =
    case reflectStage @s of
      StageResolved ->
        "[" <> mconcat (intersperse ", " (map printer (toList expressions))) <>
        "]"
      StageParsed ->
        addColumnsIfNeeded
          expressions
          typ
          ("[" <> mconcat (intersperse ", " (map printer (toList expressions))) <>
           "]")

instance Stage s =>  Printer (Variant s) where
  printer (Variant {tag, argument}) =
    printer tag <> (if not (null argument)
                       then "(" <> mconcat (intersperse ", " (map printer (toList argument))) <> ")"
                       else mempty)

instance Printer TagName where
  printer (TagName s) ="#" <> printer s

instance Stage s => Printer (Record s) where
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

instance Stage s => Printer (Infix s) where
  printer (Infix {left, global, right}) =
    "(" <> printer left <> " " <>
    (case reflectStage @s of
       StageResolved -> printer global
       StageParsed -> printer global) <>
    " " <>
    printer right <>
    ")"

instance Stage s =>  Printer (Literal s) where
  printer = \case
               NumberLiteral number -> printer number
               TextLiteral LiteralText{text} -> printText text

instance Stage s =>  Printer (Number s) where
  printer (Number {number}) = printer number

instance Stage s => Printer (Lambda s) where
  printer Lambda {location, param, body} =
    case reflectStage @s of
      StageResolved ->
        case location of
          ImplicitArgumentFor {} -> printer body
          _ -> "(" <> printer param <> ":" <> printer body <> ")"
      StageParsed -> "(" <> printer param <> ":" <> printer body <> ")"

instance Printer (Param Parsed) where
  printer Param{name} = printer name

instance Printer (Param Resolved) where
  printer Param {location} =
    Print $ do
      PrinterConfig {nameMappings} <- ask
      case M.lookup location nameMappings of
        Nothing -> pure "$"
        Just text -> runPrint (printer text)

instance Stage s => Printer (Variable s) where
  printer Variable {name, location} =
    case reflectStage @s of
      StageResolved ->
        Print $ do
          PrinterConfig {nameMappings} <- ask
          case M.lookup location nameMappings of
            Nothing ->
              runPrint
                ("$" <> printShow (coerce (deBrujinIndexNesting name) :: Int))
            Just text -> runPrint (printer text)
      StageParsed -> printer name

instance Stage s =>  Printer (Global s) where
  printer Global{name} =
    case reflectStage @s of
      StageResolved -> printer name
      StageParsed -> printer name

instance Printer (GlobalRef s) where
  printer =
    \case
      HashGlobal (Hash hash) -> "#" <> printShow hash
      FromIntegerGlobal -> "@prim:from_integer"
      FromDecimalGlobal -> "fromDecimal"
      EqualGlobal equality ->
        case equality of
          Equal -> "="
          NotEqual -> "/="
      CompareGlobal compareity ->
        case compareity of
          LessThan -> "<"
          GreaterThan -> ">"
          GreaterEqualTo -> ">="
          LessEqualTo -> "="
      NumericBinOpGlobal op -> printer op
      InstanceGlobal r -> printer r
      FunctionGlobal function -> "@prim:" <> printer function

instance Stage s => Printer (Apply s) where
  printer apply@Apply {function, argument, style} =
    case reflectStage @s of
      StageResolved ->
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
      StageParsed -> printerApply printer apply

printText :: Text -> Print
printText t =
  Print
    (pure
       (RIO.displayBytesUtf8
          (T.encodeUtf8 ("\"" <> T.replace "\"" "\"\"" t <> "\""))))

instance Printer InstanceName where
  printer =
    \case
      EqualIntegerInstance -> "<Equal Integer>"
      EqualTextInstance -> "<Equal Text>"
      EqualDecimalInstance n -> "<Equal (Decimal " <> printShow n <> ")>"
      CompareIntegerInstance -> "<Compare Integer>"
      CompareTextInstance -> "<Compare Text>"
      CompareDecimalInstance n -> "<Compare (Decimal " <> printShow n <> ")>"
      FromIntegerIntegerInstance -> "<FromInteger Integer>"
      FromIntegerDecimalInstance {} -> "<FromInteger Decimal>"
      FromDecimalDecimalInstance FromDecimalInstance { supersetPlaces
                                                     , subsetPlaces
                                                     } ->
        "<FromDecimal " <> printShow supersetPlaces <> " (Decimal " <>
        printShow subsetPlaces <>
        ")>"
      IntegerOpInstance op -> "<(" <> printer op <> ") @ Integer>"
      DecimalOpInstance nat op ->
        "<(" <> printer op <> ") @(Decimal " <>
        printer (fromIntegral nat :: Integer) <>
        ")>"

addColumnsIfNeeded :: Printer a => Vector e -> Maybe a -> Print -> Print
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
                                } -> "Decimal " <> printShow n
      _ -> "_"

-- TODO: Make much more robust.
instance Printer FieldName where
  printer (FieldName t) =
    if True -- Applying this for graph support. TODO: remove it.
            || T.any (not . printableNameChar) t
      then Print (pure (RIO.displayBytesUtf8 (L.toStrict (encode t))))
      else printer t

printableNameChar :: Char -> Bool
printableNameChar '_' = True
printableNameChar c = isAlphaNum c

instance Printer SomeNumber where
  printer = \case
               IntegerNumber i -> printer i
               DecimalNumber decimal -> Print (pure (RIO.display decimal))

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
      AccumFunction -> "array_accum"
      ScanFunction -> "array_scan"
      ReduceFunction -> "array_reduce"
      MinimumFunction -> "array_minimum"
      MaximumFunction -> "array_maximum"
      AverageFunction -> "array_average"
      LengthFunction -> "array_length"
      FindFunction -> "array_find"
      AllFunction -> "array_all"
      AnyFunction -> "array_any"
      NullFunction -> "array_null"
      RichDoc -> "rich_doc"
      RichParagraph -> "rich_paragraph"
      RichText -> "rich_text"
      RichBold -> "rich_bold"
      RichItalic -> "rich_italic"
      RichLink -> "rich_link"
      RichCell -> "rich_cell"

instance Printer NumericBinOp where
  printer =
    \case
      MulitplyOp -> "*"
      AddOp -> "+"
      SubtractOp -> "-"
      DivideOp -> "/"

printerApply :: (Expression s -> Print) -> Apply s -> Print
printerApply printer' apply =
  printer' function <> "(" <>
  mconcat (intersperse ", " (map printer' arguments)) <>
  ")"
  where (function, arguments) = uncurryApplies apply

printerApplyResolved :: Apply Resolved -> Print
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
