{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLists #-}
-- |

module CsvImportSpec where

import Inflex.Server.Csv
import Inflex.Schema
import Test.Hspec

spec :: Spec
spec =
  describe
    "Schema"
    (do it
          "Blank"
          (shouldBe
             (guessCsvSchema (File {id = 0, name = ""}) "")
             (GuessCassavaFailure "parse error (not enough input) at \"\""))
        it
          "Ints"
          (shouldBe
             (guessCsvSchema (File {id = 0, name = ""}) "int\n1\n2")
             (CsvGuessed
                (CsvImportSpec
                   { file = File {id = 0, name = ""}
                   , skipRows = 0
                   , separator = ","
                   , columns =
                       [ CsvColumn
                           { name = "int"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType =
                                        IntegerType (Required Version1)
                                    , renameTo = "int"
                                    })
                           }
                       ]
                   })))
        it
          "Mixed ints and decimals"
          (shouldBe
             (guessCsvSchema (File {id = 0, name = ""}) "mixed\n1\n2.02")
             (CsvGuessed
                (CsvImportSpec
                   { file = File {id = 0, name = ""}
                   , skipRows = 0
                   , separator = ","
                   , columns =
                       [ CsvColumn
                           { name = "mixed"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType =
                                        DecimalType 2 (Required Version1)
                                    , renameTo = "mixed"
                                    })
                           }
                       ]
                   })))
        it
          "Text"
          (shouldBe
             (guessCsvSchema (File {id = 0, name = ""}) "text\n1a\nabc")
             (CsvGuessed
                (CsvImportSpec
                   { file = File {id = 0, name = ""}
                   , skipRows = 0
                   , separator = ","
                   , columns =
                       [ CsvColumn
                           { name = "text"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType = TextType (Required Version1)
                                    , renameTo = "text"
                                    })
                           }
                       ]
                   })))
        it
          "Text and ints"
          (shouldBe
             (guessCsvSchema
                (File {id = 0, name = ""})
                "text,decimal\n1a,1\nabc,2.0")
             (CsvGuessed
                (CsvImportSpec
                   { file = File {id = 0, name = ""}
                   , skipRows = 0
                   , separator = ","
                   , columns =
                       [ CsvColumn
                           { name = "decimal"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType =
                                        DecimalType 1 (Required Version1)
                                    , renameTo = "decimal"
                                    })
                           }
                       , CsvColumn
                           { name = "text"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType = TextType (Required Version1)
                                    , renameTo = "text"
                                    })
                           }
                       ]
                   })))
        it
          "Missing text is optional"
          (shouldBe
             (guessCsvSchema
                (File {id = 0, name = ""})
                "text,misc\nfoo,misc\n,misc\nbar,misc")
             (CsvGuessed
                (CsvImportSpec
                   { file = File {id = 0, name = ""}
                   , skipRows = 0
                   , separator = ","
                   , columns =
                       [ CsvColumn
                           { name = "misc"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType = TextType (Required Version1)
                                    , renameTo = "misc"
                                    })
                           }
                       , CsvColumn
                           { name = "text"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType = TextType (Optional Version1)
                                    , renameTo = "text"
                                    })
                           }
                       ]
                   })))
        it
          "Missing ints is optionality"
          (shouldBe
             (guessCsvSchema
                (File {id = 0, name = ""})
                "int,x\n\
                    \2,1\n\
                    \,1\n\
                    \3,1")
             (CsvGuessed
                (CsvImportSpec
                   { file = File {id = 0, name = ""}
                   , skipRows = 0
                   , separator = ","
                   , columns =
                       [ CsvColumn
                           { name = "int"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType =
                                        IntegerType (Optional Version1)
                                    , renameTo = "int"
                                    })
                           }
                       , CsvColumn
                           { name = "x"
                           , action =
                               ImportAction
                                 (ImportColumn
                                    { importType =
                                        IntegerType (Required Version1)
                                    , renameTo = "x"
                                    })
                           }
                       ]
                   })))
        it
          "Mixed in one column yields text type"
          (do shouldBe
                (guessCsvSchema
                   (File {id = 0, name = ""})
                   "int\n\
                    \2\n\
                    \a")
                (CsvGuessed
                   (CsvImportSpec
                      { file = File {id = 0, name = ""}
                      , skipRows = 0
                      , separator = ","
                      , columns =
                          [ CsvColumn
                              { name = "int"
                              , action =
                                  ImportAction
                                    (ImportColumn
                                       { importType =
                                           TextType (Required Version1)
                                       , renameTo = "int"
                                       })
                              }
                          ]
                      }))))
