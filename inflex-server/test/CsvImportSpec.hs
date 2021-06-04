{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, OverloadedLists, TemplateHaskell #-}
-- |

module CsvImportSpec where

import qualified Data.ByteString.Lazy as L
import           Inflex.Schema
import           Inflex.Server.Csv
import           Match
import           System.Directory
import           Test.Hspec

spec :: Spec
spec = do
  describe "Schema" schema
  describe "Local testing on real files" locals

schema :: Spec
schema = do
  it
    "Blank"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "")
       $(match [|GuessCassavaFailure _|]))
  it
    "Ints"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "int\n1\n2")
       $(match
           [|CsvGuessed
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { action =
                              ImportAction
                                (ImportColumn
                                   {importType = IntegerType (Required _)})
                          }
                      ]
                  })|]))
  it
    "Mixed ints and decimals"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "mixed\n1\n2.02")
       $(match
           [|CsvGuessed
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { action =
                              ImportAction
                                (ImportColumn
                                   {importType = DecimalType 2 (Required _)})
                          }
                      ]
                  })|]))
  it
    "Text"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "text\n1a\nabc")
       $(match
           [|CsvGuessed
               (CsvImportSpec
                  { columns =
                      [ CsvColumn
                          { action =
                              ImportAction
                                (ImportColumn
                                   {importType = TextType (Required Version1)})
                          }
                      ]
                  })|]))
  it
    "Text and Ints"
    (shouldSatisfy
       (guessCsvSchema (File {id = 0, name = ""}) "text,decimal\n1a,1\nabc,2.0")
       $(match
           [|CsvGuessed
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
                  })|]))
  it
    "Missing text is optional"
    (shouldSatisfy
       (guessCsvSchema
          (File {id = 0, name = ""})
          "text,misc\nfoo,misc\n,misc\nbar,misc")
       $(match
           [|CsvGuessed
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
                  })|]))
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
           [|CsvGuessed
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
                  })|]))
  it
    "Mixed in one column yields text type"
    (do shouldSatisfy
          (guessCsvSchema
             (File {id = 0, name = ""})
             "int\n\
                        \2\n\
                        \a")
          $(match
              [|CsvGuessed
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
                     })|]))

-- We test on real files, but we don't check the files in. So these
-- tests are marked pending.
locals :: Spec
locals =
  it
    "Monzo 5k row export"
    (untrackedFileShouldSatisfy
       "/home/chris/Downloads/Monzo Data Export - CSV (Friday, February 26th, 2021).csv"
       $(match [|CsvGuessed _|]))

-- | If the file exists, parse it and test that it matches. If it
-- doesn't exist, mark the test pending and ignore it.
untrackedFileShouldSatisfy :: HasCallStack => FilePath -> (CsvGuess -> Bool) -> IO ()
untrackedFileShouldSatisfy fp res = do
  exists <- doesFileExist fp
  if exists
    then do
      bytes <- L.readFile fp
      shouldSatisfy (guessCsvSchema (File {id = 0, name = ""}) bytes) res
    else pendingWith "File doesn't exist, so skipping."
