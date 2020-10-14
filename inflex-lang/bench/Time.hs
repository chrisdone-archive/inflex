{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID as UUID
import           Data.UUID.V4
import           Gauge.Main
import           Inflex.Document
import           Inflex.Generaliser
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.Parser
import           Inflex.Renamer
import           Inflex.Resolver
import           Inflex.Solver
import           Inflex.Types

main :: IO ()
main = do
  u1 <- nextRandom'
  let !mediumArray = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "]"]
      !mediumArray2 = T.concat ["[", T.intercalate "," (replicate 2000 "1"), "]"]
      !mediumArray3 = T.concat ["[", T.intercalate "," (replicate 4000 "1"), "]"]
  defaultMain
    [ bgroup
        "parseText"
        [bench "medium array" (nf parseTextUpToErrorSuccess mediumArray)]
    , bgroup
        "renameText"
        [bench "medium array" (nf renameTextUpToErrorSuccess mediumArray)]
    , bgroup
        "generateText"
        [bench "medium array" (nf generateTextUpToErrorSuccess mediumArray)]
    , bgroup
        "solveText"
        [bench "medium array" (nf solveTextUpToErrorSuccess mediumArray),
        bench "medium array2" (nf solveTextUpToErrorSuccess mediumArray2),
        bench "medium array3" (nf solveTextUpToErrorSuccess mediumArray3)]
    , bgroup
        "generaliseText"
        [bench "medium array" (nf generaliseTextUpToErrorSuccess mediumArray)]
    , bgroup
        "resolveText"
        [bench "medium array" (nf resolveTextUpToErrorSuccess mediumArray)]
    , bgroup
        "loadDocument"
        [ bench
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
        ]
    ]

nextRandom' :: IO Text
nextRandom' = fmap UUID.toText nextRandom

loadDocumentUpToErrorSuccess :: [Named Text] -> Toposorted (Named (Either () ()))
loadDocumentUpToErrorSuccess = fmap (fmap (first (const ()) . second (const ()))) . loadDocument

parseTextUpToErrorSuccess :: Text -> Either () ()
parseTextUpToErrorSuccess = first (const ()) . second (const ()) . parseText ""

renameTextUpToErrorSuccess :: Text -> Either () ()
renameTextUpToErrorSuccess = first (const ()) . second (const ()) . renameText ""

solveTextUpToErrorSuccess :: Text -> Either () ()
solveTextUpToErrorSuccess = first (const ()) . second (const ()) . solveText mempty ""

generateTextUpToErrorSuccess :: Text -> Either () ()
generateTextUpToErrorSuccess = first (const ()) . second (const ()) . generateText mempty ""

generaliseTextUpToErrorSuccess :: Text -> Either () ()
generaliseTextUpToErrorSuccess = first (const ()) . second (const ()) . generaliseText mempty ""

resolveTextUpToErrorSuccess :: Text -> Either () ()
resolveTextUpToErrorSuccess = first (const ()) . second (const ()) . resolveText mempty ""
