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
  | MissingRequiredField

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
                  parseColumnText file importType value))
    rows
  where
    columnMap =
      HM.fromList
        (map (\CsvColumn {name, action} -> (name, action)) (V.toList columns))

parseColumnText ::
     File -> CsvColumnType -> Text -> Either ImportError (Maybe (Expression Parsed))
parseColumnText File {name} typ text =
  case typ of
    IntegerType optionality ->
      if T.null (T.strip text)
        then checkRequired optionality
        else case parseTextWith numberExpressionParser text of
               Left err -> Left (ParseFieldError typ err)
               Right expr -> pure (Just expr)
    DecimalType _ optionality ->
      if T.null (T.strip text)
        then checkRequired optionality
        else case parseTextWith numberExpressionParser text of
               Left err -> Left (ParseFieldError typ err)
               Right expr -> pure (Just expr)
    TextType optionality ->
      if T.null (T.strip text)
        then checkRequired optionality
        else pure
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
    checkRequired Required {} = Left MissingRequiredField
    checkRequired Optional {} = pure Nothing
    emptyPos = SourcePos {line = 0, column = 0, name = T.unpack name}

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
              (fromMaybe (TextType required) . foldl' typeAndSeenToType Nothing)
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

-- | For easy folding: if we have two types, combine them, else use
-- whatever type we see, if anything.
typeAndSeenToType :: Maybe CsvColumnType -> Seen -> Maybe CsvColumnType
typeAndSeenToType mtype seen =
  case mtype of
    Nothing -> seenToType seen
    Just typ ->
      case seenToType seen of
        Nothing -> pure typ
        Just typ2 -> pure (combineTypes typ typ2)

-- | Give a possible type to a seen thing.
seenToType :: Seen -> Maybe CsvColumnType
seenToType =
  \case
    SeenNothing -> Nothing
    SeenText -> pure (TextType required)
    SeenDecimal natural ->
      pure
        (DecimalType
           (fromIntegral natural -- TODO: bad
            )
           required)
    SeenInteger -> pure (IntegerType required)

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

-- | Handy helper.
required :: Optionality
required = Required versionRefl

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
