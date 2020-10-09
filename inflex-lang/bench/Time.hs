{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID as UUID
import           Data.UUID.V4
import           Gauge.Main
import           Inflex.Document
import           Inflex.Instances ()
import           Inflex.Types

main :: IO ()
main = do
  u1 <- nextRandom'
  let !smallArray = T.concat ["[", T.intercalate "," (replicate 10 "1"), "]"]
  let !mediumArray = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "]"]
  -- let !largeArray =
  --       T.concat ["[", T.intercalate "," (replicate 10000 "1"), "]"]
  defaultMain
    [ bgroup
        "loadDocument"
        [ bench
            "baseline"
            (nf
               loadDocumentUpToErrorSuccess
               [ Named
                   { uuid = Uuid u1
                   , name = "x"
                   , thing = "0"
                   , order = 0
                   , code = "0"
                   }
               ])
        , bench
            "arithmetic"
            (nf
               loadDocumentUpToErrorSuccess
               [ Named
                   { uuid = Uuid u1
                   , name = "x"
                   , thing = "1 * 2"
                   , order = 0
                   , code = "1 * 2"
                   }
               ])
        , bench
            "small array"
            (nf
               loadDocumentUpToErrorSuccess
               [ Named
                   { uuid = Uuid u1
                   , name = "x"
                   , thing = smallArray
                   , order = 0
                   , code = smallArray
                   }
               ])
        , bench
            "medium array"
            (nf
               loadDocumentUpToErrorSuccess
               [ Named
                   { uuid = Uuid u1
                   , name = "x"
                   , thing = mediumArray
                   , order = 0
                   , code = mediumArray
                   }
               ])
         {-bench
            "large array"
            (nf
               loadDocumentUpToErrorSuccess
               [ Named
                   { uuid = Uuid u1
                   , name = "x"
                   , thing = largeArray
                   , order = 0
                   , code = largeArray
                   }
               ])-}
        ]
    ]

nextRandom' :: IO Text
nextRandom' = fmap UUID.toText nextRandom

loadDocumentUpToErrorSuccess :: [Named Text] -> Toposorted (Named (Either () ()))
loadDocumentUpToErrorSuccess = fmap (fmap (first (const ()) . second (const ()))) . loadDocument
