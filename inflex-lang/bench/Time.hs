{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID as UUID
import           Data.UUID.V4
import           Gauge.Main
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.Parser
import           Inflex.Solver
import qualified RIO
import           RIO (newSomeRef, RIO)

{-

with optimisations with type signature

[0Kbenchmarked parseText/medium array
time                 7.805 ms   (7.351 ms .. 8.342 ms)
                     0.983 R²   (0.967 R² .. 0.999 R²)
mean                 7.479 ms   (7.379 ms .. 7.673 ms)
std dev              385.7 μs   (232.3 μs .. 677.0 μs)
variance introduced by outliers: 25% (moderately inflated)

[0Kbenchmarked generateText/medium array
time                 12.89 ms   (12.19 ms .. 13.59 ms)
                     0.978 R²   (0.954 R² .. 0.991 R²)
mean                 14.50 ms   (14.04 ms .. 15.07 ms)
std dev              1.322 ms   (1.082 ms .. 1.652 ms)
variance introduced by outliers: 44% (moderately inflated)

[0Kbenchmarked solveText/array[1000]
time                 18.46 ms   (18.16 ms .. 18.78 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 18.64 ms   (18.43 ms .. 18.93 ms)
std dev              635.0 μs   (432.2 μs .. 920.5 μs)

-}

main :: IO ()
main = do
  -- u1 <- nextRandom'
  let !mediumArray = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "] :: [Integer]"]
      -- !array2000 = T.concat ["[", T.intercalate "," (replicate 2000 "1"), "]:: [Integer]"]
      -- !array4000 = T.concat ["[", T.intercalate "," (replicate 4000 "1"), "]:: [Integer]"]
  defaultMain
    [ bgroup
        "parseText"
        [bench "medium array" (nf parseTextUpToErrorSuccess mediumArray)]
    -- , bgroup
    --     "renameText"
    --     [bench "medium array" (nf renameTextUpToErrorSuccess mediumArray)]
    , bgroup
        "generateText"
        [bench "medium array" (nf generateTextUpToErrorSuccess mediumArray)]
    , bgroup
        "solveText"
        [bench "array[1000]" (nfIO (do ref <- newSomeRef 0;binds <- newSomeRef mempty
                                       RIO.runRIO (SolveReader {glogfunc = mempty, counter = ref,binds}) (solveTextUpToErrorSuccess mediumArray)))]
   -- , bgroup
   --     "solveText"
   --     [bench "array[2000]" (nfIO (do ref <- newSomeRef 0;binds <- newSomeRef mempty
   --                                    RIO.runRIO (SolveReader {glogfunc = mempty, counter = ref,binds}) (solveTextUpToErrorSuccess array2000)))]
   --  , bgroup
   --      "solveText"
   --      [bench "array[4000]" (nfIO (do ref <- newSomeRef 0;binds <- newSomeRef mempty
   --                                     RIO.runRIO (SolveReader {glogfunc = mempty, counter = ref,binds}) (solveTextUpToErrorSuccess array4000)))]
    -- , bgroup
    --     "generaliseText"
    --     [bench "medium array" (nf generaliseTextUpToErrorSuccess mediumArray)]
    -- , bgroup
    --     "resolveText"
    --     [bench "medium array" (nf resolveTextUpToErrorSuccess mediumArray)]
    -- , bgroup
    --     "loadDocument"
    --     [ bench
    --         "medium array"
    --         (nf
    --            loadDocumentUpToErrorSuccess
    --            [ Named
    --                { uuid = Uuid u1
    --                , name = "x"
    --                , thing = mediumArray
    --                , order = 0
    --                , code = mediumArray
    --                }
    --            ])
    --     ]
    ]

-- nextRandom' :: IO Text
-- nextRandom' = fmap UUID.toText nextRandom

-- loadDocumentUpToErrorSuccess :: [Named Text] -> Toposorted (Named (Either () ()))
-- loadDocumentUpToErrorSuccess = fmap (fmap (first undefined . second (const ()))) . loadDocument

parseTextUpToErrorSuccess :: Text -> Either () ()
parseTextUpToErrorSuccess = first (const ()) . second (const ()) . parseText ""

-- renameTextUpToErrorSuccess :: Text -> Either () ()
-- renameTextUpToErrorSuccess = first (const ()) . second (const ()) . renameText ""

solveTextUpToErrorSuccess :: Text -> RIO SolveReader (Either () ())
solveTextUpToErrorSuccess = fmap (bimap (const ()) (const ())) . solveText mempty ""

generateTextUpToErrorSuccess :: Text -> Either () ()
generateTextUpToErrorSuccess = bimap (const ()) (const ())  . generateText mempty ""

-- generaliseTextUpToErrorSuccess :: Text -> Either () ()
-- generaliseTextUpToErrorSuccess = first (const ()) . second (const ()) . generaliseText mempty ""

-- resolveTextUpToErrorSuccess :: Text -> Either () ()
-- resolveTextUpToErrorSuccess = first (const ()) . second (const ()) . resolveText mempty ""
