{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

import           Data.Bifunctor
import           Data.Text (Text)
import qualified Data.Text as T
import           Gauge.Main
import           Inflex.Generator
import           Inflex.Instances ()
import           Inflex.Parser
import           Inflex.Solver
import qualified RIO
import           RIO (newSomeRef, RIO)

{-

with optimisations with type signature

Benchmark inflex-lang-time: RUNNING...
[0Kbenchmarked parseText/medium array
time                 9.687 ms   (8.865 ms .. 10.32 ms)
                     0.965 R²   (0.943 R² .. 0.983 R²)
mean                 8.730 ms   (8.423 ms .. 9.049 ms)
std dev              899.5 μs   (754.3 μs .. 1.070 ms)
variance introduced by outliers: 56% (severely inflated)

[0Kbenchmarked generateText/medium array
time                 16.02 ms   (13.44 ms .. 18.07 ms)
                     0.932 R²   (0.886 R² .. 0.974 R²)
mean                 17.79 ms   (16.81 ms .. 19.41 ms)
std dev              3.108 ms   (1.894 ms .. 5.119 ms)
variance introduced by outliers: 73% (severely inflated)

[0Kbenchmarked solveText/array[1000]
time                 16.88 ms   (16.25 ms .. 17.70 ms)
                     0.994 R²   (0.991 R² .. 0.998 R²)
mean                 19.17 ms   (18.19 ms .. 21.85 ms)
std dev              3.797 ms   (1.954 ms .. 6.808 ms)
variance introduced by outliers: 81% (severely inflated)

benchmarking solveText/array[1000] no sig ... took 9.582 s, total 56 iterations
[0Kbenchmarked solveText/array[1000] no sig
time                 190.0 ms   (173.9 ms .. 213.2 ms)
                     0.983 R²   (0.962 R² .. 0.998 R²)
mean                 167.0 ms   (156.7 ms .. 177.1 ms)
std dev              17.35 ms   (12.26 ms .. 23.62 ms)
variance introduced by outliers: 29% (moderately inflated)

-}

main :: IO ()
main = do
  -- u1 <- nextRandom'
  let !mediumArray = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "] :: [Integer]"]
      !mediumArraynosig = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "]"]
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
    , bgroup
        "solveText"
        [bench "array[1000] no sig" (nfIO (do ref <- newSomeRef 0;binds <- newSomeRef mempty
                                              RIO.runRIO (SolveReader {glogfunc = mempty, counter = ref,binds}) (solveTextUpToErrorSuccess mediumArraynosig)))]
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
