{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, OverloadedLists, TemplateHaskell #-}
-- |

module CsvImportSpec where

import           Data.Bifunctor
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import           Inflex.Schema
import           Inflex.Display ()
import           Inflex.Server.Csv
import           Inflex.Types
import           Match
import qualified RIO
import           System.Directory
import           Test.Hspec

spec :: Spec
spec = do
  describe "Schema" schema
  describe "Local testing on real files" locals
  describe "Parsed" parsed
  describe "Printed" printed

schema :: Spec
schema = do
  it
    "Blank"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "")
       $(match [|Left _|]))
  it
    "Ints"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "int\n1\n2")
       $(match
           [|Right
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { action =
                              ImportAction
                                (ImportColumn
                                   {importType = IntegerType (Required _)})
                          }
                      ]
                  }, _)|]))
  it
    "Mixed ints and decimals"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "mixed\n1\n2.02")
       $(match
           [|Right
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { action =
                              ImportAction
                                (ImportColumn
                                   {importType = DecimalType 2 (Required _)})
                          }
                      ]
                  }, _)|]))
  it
    "Text"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "text\n1a\nabc")
       $(match
           [|Right
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { action =
                              ImportAction
                                (ImportColumn
                                   {importType = TextType (Required Version1)})
                          }
                      ]
                  }, _)|]))
  it
    "Text and Ints"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "text,decimal\n1a,1\nabc,2.0")
       $(match
           [|Right
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { name = "text"
                          , action =
                              ImportAction
                                (ImportColumn
                                   {importType = TextType (Required Version1)})
                          }
                      , CsvColumn
                          { name = "decimal"
                          , action =
                              ImportAction
                                (ImportColumn
                                   { importType =
                                       DecimalType 1 (Required Version1)
                                   })
                          }
                      ]
                  }, _)|]))
  it
    "Missing text is optional"
    (shouldSatisfy
       (guessCsvSchema
          (File {id = 0, name = ""})
          "text,misc\nfoo,misc\n,misc\nbar,misc")
       $(match
           [|Right
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { name = "text"
                          , action =
                              ImportAction
                                (ImportColumn
                                   {importType = TextType (Optional Version1)})
                          }
                      , CsvColumn
                          { name = "misc"
                          , action =
                              ImportAction
                                (ImportColumn
                                   {importType = TextType (Required Version1)})
                          }
                      ]
                  }, _)|]))
  it
    "Missing ints is optionality"
    (shouldSatisfy
       (guessCsvSchema
          (File {id = 0, name = ""})
          "int,x\n\
                        \2,1\n\
                        \,1\n\
                        \3,1")
       $(match
           [|Right
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { name = "int"
                          , action =
                              ImportAction
                                (ImportColumn
                                   { importType =
                                       IntegerType (Optional Version1)
                                   })
                          }
                      , CsvColumn
                          { name = "x"
                          , action =
                              ImportAction
                                (ImportColumn
                                   { importType =
                                       IntegerType (Required Version1)
                                   })
                          }
                      ]
                  }, _)|]))
  it
    "Mixed in one column yields text type"
    (do shouldSatisfy
          (guessCsvSchema
             (File {id = 0, name = ""})
             "int\n\
                        \2\n\
                        \a")
          $(match
              [|Right
                  (CsvImportSpec
                     { columns =
                         [ CsvColumn
                             { action =
                                 ImportAction
                                   (ImportColumn
                                      { importType =
                                          TextType (Required Version1)
                                      })
                             }
                         ]
                     }, _)|]))

-- We test on real files, but we don't check the files in. So these
-- tests are marked pending.
locals :: Spec
locals =
  it
    "Monzo 5k row export"
    (untrackedFileShouldSatisfy
       "/home/chris/Downloads/Monzo Data Export - CSV (Friday, February 26th, 2021).csv"
       $(match [|CsvGuessed _|]))

printed :: Spec
printed =
  it
    "Small sample"
    (shouldSatisfy
       (printGuessed "name,age\nDave,123\nMary,456\n")
       $(match
           [|Right
               "[{\"name\": \"Dave\", \"age\": 123}, {\"name\": \"Mary\", \"age\": 456}]\
               \ :: [{\"name\":Text, \"age\":Integer}]"|]))

parsed :: Spec
parsed =
  it
    "Small sample"
    (shouldSatisfy
       (guessAndParseArray "name,age\nDave,123\nMary,456\n")
       $(match
           [|Right
               (Array
                  { expressions =
                      [ RecordExpression
                          (Record
                             { fields =
                                 [ FieldE
                                     { name = FieldName {unFieldName = "name"}
                                     , expression =
                                         LiteralExpression
                                           (TextLiteral
                                              (LiteralText
                                                 {text = "Dave", typ = Nothing}))
                                     }
                                 , FieldE
                                     { name = FieldName {unFieldName = "age"}
                                     , expression =
                                         LiteralExpression
                                           (NumberLiteral
                                              (Number
                                                 { number = IntegerNumber 123
                                                 , typ = Nothing
                                                 }))
                                     }
                                 ]
                             , typ = Nothing
                             })
                      , RecordExpression
                          (Record
                             { fields =
                                 [ FieldE
                                     { name = FieldName {unFieldName = "name"}
                                     , expression =
                                         LiteralExpression
                                           (TextLiteral
                                              (LiteralText
                                                 {text = "Mary", typ = Nothing}))
                                     }
                                 , FieldE
                                     { name = FieldName {unFieldName = "age"}
                                     , expression =
                                         LiteralExpression
                                           (NumberLiteral
                                              (Number
                                                 { number = IntegerNumber 456
                                                 , typ = Nothing
                                                 }))
                                     }
                                 ]
                             , typ = Nothing
                             })
                      ]
                  , typ =
                      Just
                        (ArrayType
                           (RecordType
                              (RowType
                                 (TypeRow
                                    { typeVariable = Nothing
                                    , fields =
                                        [ Field
                                            { name =
                                                FieldName {unFieldName = "name"}
                                            , typ =
                                                ConstantType
                                                  (TypeConstant
                                                     {name = TextTypeName})
                                            }
                                        , Field
                                            { name =
                                                FieldName {unFieldName = "age"}
                                            , typ =
                                                ConstantType
                                                  (TypeConstant
                                                     {name = IntegerTypeName})
                                            }
                                        ]
                                    }))))
                  })|]))

--------------------------------------------------------------------------------
-- Utilities

printGuessed :: L.ByteString -> Either () Text
printGuessed bs = do
  arr <- guessAndParseArray bs
  pure (RIO.textDisplay arr)

guessAndParseArray :: L.ByteString -> Either () (Array Parsed)
guessAndParseArray bs = do
  (schema', rows) <- first (const ()) (guessCsvSchema file' bs)
  rows' <- first (const ()) (importViaSchema file' schema' rows)
  pure (rowsToArray schema' rows')
  where
    file' = File {id = 0, name = ""}

-- | If the file exists, parse it and test that it matches. If it
-- doesn't exist, mark the test pending and ignore it.
untrackedFileShouldSatisfy :: HasCallStack => FilePath -> (CsvGuess -> Bool) -> IO ()
untrackedFileShouldSatisfy fp res = do
  exists <- doesFileExist fp
  if exists
    then do
      bytes <- L.readFile fp
      shouldSatisfy
        (case guessCsvSchema file' bytes of
           Left err -> GuessCassavaFailure err
           Right (schema', _rows) -> CsvGuessed schema')
        res
    else pendingWith "File doesn't exist, so skipping."
  where
    file' = File {id = 0, name = ""}
