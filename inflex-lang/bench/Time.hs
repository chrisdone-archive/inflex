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

Latest numbers:

[0Kbenchmarked parseText/medium array
time                 21.35 ms   (21.19 ms .. 21.51 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 21.26 ms   (21.15 ms .. 21.38 ms)
std dev              266.1 μs   (187.0 μs .. 408.3 μs)

[0Kbenchmarked generateText/medium array
time                 36.31 ms   (36.00 ms .. 36.73 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 37.42 ms   (36.77 ms .. 40.41 ms)
std dev              2.372 ms   (278.0 μs .. 4.665 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking solveText/medium array ... took 30.40 s, total 56 iterations
[0Kbenchmarked solveText/medium array
time                 548.2 ms   (540.3 ms .. 569.9 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 553.0 ms   (549.4 ms .. 562.2 ms)
std dev              8.898 ms   (3.145 ms .. 14.24 ms)

WITH TYPE SIGNATURE

benchmarking solveText/medium array ... took 17.01 s, total 56 iterations
[0Kbenchmarked solveText/medium array
time                 312.1 ms   (307.4 ms .. 315.8 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 308.1 ms   (306.2 ms .. 310.0 ms)
std dev              3.175 ms   (2.390 ms .. 4.474 ms)

-}

main :: IO ()
main = do
  -- u1 <- nextRandom'
  let !mediumArray = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "] :: [Integer]"]
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
        [bench "medium array" (nfIO (do ref <- newSomeRef 0
                                        RIO.runRIO (SolveReader {glogfunc = mempty, counter = ref}) (solveTextUpToErrorSuccess mediumArray)))]
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

nextRandom' :: IO Text
nextRandom' = fmap UUID.toText nextRandom

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
