{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
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
  , rowsToArray
  , hashMapToOMap
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OM
import           Data.Hashable
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Inflex.Lexer
import           Inflex.Parser
import           Inflex.Schema
import           Inflex.Type (maybeType)
import           Inflex.Types
import           Numeric.Natural

--------------------------------------------------------------------------------
-- Schema-based importing

data ImportError
  = MissingKey Text
  | ParseFieldError CsvColumnType LexParseError
  | MissingRequiredField Text
  deriving (Show)

--
-- TODO: Parallelism?
importViaSchema ::
     File
  -> CsvImportSpec
  -> Vector (InsOrdHashMap Text Text)
  -> Either ImportError (Vector (InsOrdHashMap Text (Expression Parsed)))
importViaSchema file CsvImportSpec {columns} rows =
  traverse
    (fmap (OM.mapMaybe Prelude.id) .
     OM.traverseWithKey
       (\key value ->
          case OM.lookup key columnMap of
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
      OM.fromList
        (map (\CsvColumn {name, action} -> (name, action)) (V.toList columns))

rowsToArray :: CsvImportSpec -> Vector (InsOrdHashMap Text (Expression Parsed)) -> Array Parsed
rowsToArray CsvImportSpec {columns = cols} vs =
  Array
    { expressions =
        fmap
          (\hash' ->
             RecordExpression
               Record
                 { fields =
                     mapMaybe
                       (\CsvColumn {name, action} ->
                          case action of
                            IgnoreColumn -> Nothing
                            ImportAction ImportColumn {renameTo} -> do
                              expression <- OM.lookup name hash'
                              pure
                                FieldE
                                  { name = FieldName renameTo
                                  , expression
                                  , location
                                  })
                       (toList cols)
                 , location
                 , typ = Nothing
                 })
          vs
    , typ =
        Just
          (ArrayType
             (RecordType
                (RowType
                   TypeRow
                     { typeVariable = Nothing
                     , fields =
                         mapMaybe
                           (\CsvColumn {action} ->
                              case action of
                                IgnoreColumn -> Nothing
                                ImportAction ImportColumn {importType, renameTo} ->
                                  pure
                                    Field
                                      { location
                                      , name = FieldName renameTo
                                      , typ = csvTypeToRealType importType
                                      })
                           (toList cols)
                     , location
                     })))
    , location
    }
  where
    location =
      SourceLocation
        { start = SourcePos {line = 0, column = 0, name = ""}
        , end = SourcePos {line = 0, column = 0, name = ""}
        }

csvTypeToRealType :: CsvColumnType -> Type Parsed
csvTypeToRealType =
  \case
    IntegerType optionality ->
      case optionality of
        Required {} -> integerT
        Optional {} -> maybeType ["none"] location integerT
    TextType optionality ->
      case optionality of
        Required {} -> textT
        Optional {} -> maybeType ["none"] location textT
    DecimalType n optionality ->
      case optionality of
        Required {} -> decimalT (fromIntegral n)
        Optional {} -> maybeType ["none"] location (decimalT (fromIntegral n))
  where
    location =
      SourceLocation
        { start = SourcePos {line = 0, column = 0, name = ""}
        , end = SourcePos {line = 0, column = 0, name = ""}
        }
    integerT =
      ConstantType
        TypeConstant {location, name = IntegerTypeName}

    textT =
      ConstantType
        TypeConstant {location, name = TextTypeName}
    decimalT nat =
      ApplyType
        TypeApplication
          { function =
              ConstantType TypeConstant {location, name = DecimalTypeName}
          , argument =
              ConstantType TypeConstant {location, name = NatTypeName nat}
          , location
          , kind = TypeKind
          }

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

hashMapToOMap :: (Ord k, Hashable k, Foldable f) => f k -> HashMap k a -> InsOrdHashMap k a
hashMapToOMap order origin =
  foldl'
    (\hm k ->
       case HM.lookup k origin of
         Nothing -> hm
         Just v -> OM.insert k v hm)
    mempty
    order

-- | Make a pretty good guess at the schema of a CSV file.
--
-- TODO: Parallelism?
guessCsvSchema ::
     File
  -> L.ByteString
  -> Either Text (CsvImportSpec, Vector (InsOrdHashMap Text Text))
guessCsvSchema file bytes =
  case Csv.decodeByName bytes of
    Left err -> Left (T.pack err)
    Right (headers, rows :: Vector (HashMap Text Text)) ->
      let insOrdRows = fmap (hashMapToOMap (fmap T.decodeUtf8 headers)) rows
          seenRows = fmap (fmap (pure . see)) insOrdRows
          combinedRows :: InsOrdHashMap Text (Seq Seen)
          combinedRows = foldl' (OM.unionWith (<>)) mempty seenRows
          -- Above: foldl' is correct direction, checked:
          -- https://hackage.haskell.org/package/containers-0.6.4.1/docs/src/Data.Map.Strict.Internal.html#unionsWith
          typedRows :: InsOrdHashMap Text CsvColumnType
          typedRows =
            fmap
              (fromMaybe (TextType required) .
               iffyType . foldl' typeAndSeenToType NoneSoFar)
              combinedRows
       in Right
            ( CsvImportSpec
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
                         (OM.toList typedRows))
                , file
                }
            , insOrdRows)

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
