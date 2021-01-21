{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Inflex.Server.Csv
  ( guessCsvSchema
  , ImportError(..)
  , importViaSchema
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Inflex.Lexer
import           Inflex.Parser
import           Inflex.Schema
import           Inflex.Types
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Schema-based importing

data ImportError
  = MissingKey Text
  | ParseFieldError CsvColumnType LexParseError
  | MissingRequiredField Text
  deriving (Show)

importViaSchema ::
     File
  -> CsvImportSpec
  -> Vector (HashMap Text Text)
  -> Either ImportError (Vector (HashMap Text (Expression Parsed)))
importViaSchema file CsvImportSpec {columns} rows =
  traverse
    (fmap (HM.mapMaybe Prelude.id) .
     HM.traverseWithKey
       (\key value ->
          case HM.lookup key columnMap of
            Nothing -> Left (MissingKey key)
            Just action ->
              case action of
                IgnoreColumn -> pure Nothing
                ImportAction ImportColumn { importType
                                          , renameTo = _ -- TODO:
                                          } ->
                  fmap Just (parseColumnText file importType key value)))
    rows
  where
    columnMap =
      HM.fromList
        (map (\CsvColumn {name, action} -> (name, action)) (V.toList columns))

parseColumnText ::
     File -> CsvColumnType -> Text -> Text -> Either ImportError (Expression Parsed)
parseColumnText _file typ key text =
  case typ of
    IntegerType optionality ->
      if T.null (T.strip text)
        then viaOptionality optionality Nothing
        else case parseTextWith numberExpressionParser text of
               Left err -> Left (ParseFieldError typ err)
               Right expr -> viaOptionality optionality (Just expr)
    DecimalType _ optionality ->
      if T.null (T.strip text)
        then viaOptionality optionality Nothing
        else case parseTextWith numberExpressionParser text of
               Left err -> Left (ParseFieldError typ err)
               Right expr -> viaOptionality optionality (Just expr)
    TextType optionality ->
      if T.null (T.strip text) && isOptional optionality
        then viaOptionality optionality Nothing
        else viaOptionality
               optionality
               (Just
                  (LiteralExpression
                     (TextLiteral
                        (LiteralText
                           { typ = Nothing
                           , text
                           , location =
                               SourceLocation {start = emptyPos, end = emptyPos}
                           }))))
  where
    viaOptionality Required {} Nothing = Left (MissingRequiredField key)
    viaOptionality Required {} (Just e) = Right e
    viaOptionality Optional {} Nothing = pure (optionalExpression Nothing)
    viaOptionality Optional {} (Just e) = pure (optionalExpression (Just e))
    isOptional Optional {} = True
    isOptional _ = False

emptyPos :: SourcePos
emptyPos = SourcePos {line = 0, column = 0, name = "none"}

optionalExpression :: Maybe (Expression Parsed) -> Expression Parsed
optionalExpression Nothing =
  VariantExpression
    Variant
      { location = SourceLocation {start = emptyPos, end = emptyPos}
      , typ = Nothing
      , tag = TagName "none"
      , argument = Nothing
      }
optionalExpression (Just e) =
  VariantExpression
    Variant
      { location = SourceLocation {start = emptyPos, end = emptyPos}
      , typ = Nothing
      , tag = TagName "ok"
      , argument = Just e
      }

--------------------------------------------------------------------------------
-- Schema guessing

-- | Make a pretty good guess at the schema of a CSV file.
guessCsvSchema :: File -> L.ByteString -> CsvGuess
guessCsvSchema file bytes =
  case Csv.decodeByName bytes of
    Left err -> GuessCassavaFailure (T.pack err)
    Right (_headers, rows :: Vector (Map Text Text)) ->
      let seenRows = fmap (fmap (pure . see)) rows
          combinedRows :: Map Text (Seq Seen)
          combinedRows = M.unionsWith (<>) seenRows
          typedRows :: Map Text CsvColumnType
          typedRows =
            fmap
              (fromMaybe (TextType required) .
               iffyType . foldl' typeAndSeenToType NoneSoFar)
              combinedRows
       in CsvGuessed
            (CsvImportSpec
               { skipRows = 0
               , separator = ","
               , columns =
                   V.fromList
                     (map
                        (\(name, importType) ->
                           CsvColumn
                             { name
                             , action =
                                 ImportAction
                                   ImportColumn {importType, renameTo = name}
                             })
                        (M.toList typedRows))
               , file
               })

--------------------------------------------------------------------------------
-- Synthesis of seen features into a type

data Iffy = NoneSoFar | FoundEmpty | Found CsvColumnType

iffyType :: Iffy -> Maybe CsvColumnType
iffyType =
  \case
    Found t -> pure t
    _ -> Nothing

-- | For easy folding: if we have two types, combine them, else use
-- whatever type we see, if anything.
typeAndSeenToType :: Iffy -> Seen -> Iffy
typeAndSeenToType iffy = combineIffy iffy . seenToType

-- | Give a possible type to a seen thing.
seenToType :: Seen -> Iffy
seenToType =
  \case
    SeenNothing -> FoundEmpty
    SeenText -> Found (TextType required)
    SeenDecimal natural ->
      Found
        (DecimalType
           (fromIntegral natural -- TODO: bad
            )
           required)
    SeenInteger -> Found (IntegerType required)

-- | Combine types that are "iffy" -- maybe we have them, maybe we
-- don't. But "nothing seen so far" is different to "saw an empty
-- column".
combineIffy :: Iffy -> Iffy -> Iffy
combineIffy NoneSoFar x = x
combineIffy x NoneSoFar = x
combineIffy FoundEmpty FoundEmpty = FoundEmpty
combineIffy FoundEmpty (Found csvColumnType) = Found (setOptional csvColumnType)
combineIffy (Found csvColumnType) FoundEmpty = Found (setOptional csvColumnType)
combineIffy (Found t1) (Found t2) = Found (combineTypes t1 t2)

-- | Combines likewise types as-is, integer is upgraded to decimal, and all types
-- upgrade to text.
combineTypes :: CsvColumnType -> CsvColumnType -> CsvColumnType
combineTypes (IntegerType o1) (IntegerType o2) =
  IntegerType (combineOptionalities o1 o2)
combineTypes (IntegerType o1) (DecimalType places o2) =
  DecimalType places (combineOptionalities o1 o2)
combineTypes (DecimalType places o2) (IntegerType o1) =
  DecimalType places (combineOptionalities o1 o2)
combineTypes (DecimalType places1 o2) (DecimalType places2 o1) =
  DecimalType (max places1 places2) (combineOptionalities o1 o2)
combineTypes (TextType o2) (DecimalType _ o1) =
  TextType (combineOptionalities o1 o2)
combineTypes (DecimalType _ o1) (TextType o2) =
  TextType (combineOptionalities o1 o2)
combineTypes (TextType o2) (IntegerType o1) =
  TextType (combineOptionalities o1 o2)
combineTypes (IntegerType o1) (TextType o2) =
  TextType (combineOptionalities o1 o2)
combineTypes (TextType o1) (TextType o2) =
  TextType (combineOptionalities o1 o2)

-- | Merging optionality (optional overrides required).
combineOptionalities :: Optionality -> Optionality -> Optionality
combineOptionalities Optional {} _ = Optional versionRefl
combineOptionalities _ Optional {} = Optional versionRefl
combineOptionalities Required {} Required {} = Required versionRefl

setOptional :: CsvColumnType -> CsvColumnType
setOptional (IntegerType (_ :: Optionality)) = IntegerType optional
setOptional (DecimalType p (_ :: Optionality)) = DecimalType p optional
setOptional (TextType (_ :: Optionality)) = TextType optional

-- | Handy helper.
required :: Optionality
required = Required versionRefl

-- | Handy helper.
optional :: Optionality
optional = Optional versionRefl

--------------------------------------------------------------------------------
-- Look at the text in a column

data Seen
  = SeenNothing
  | SeenText
  | SeenDecimal Natural
  | SeenInteger

-- | Report what we see in a column.
see :: Text -> Seen
see t =
  if T.null (T.strip t)
    then SeenNothing
    else case lexTextSingleton t of
           Right Located {thing = token}
             | NaturalToken {} <- token -> SeenInteger
             | DecimalToken Decimal {places} <- token -> SeenDecimal places
           _ -> SeenText
