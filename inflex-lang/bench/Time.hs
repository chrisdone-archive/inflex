{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

import           Control.Monad
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

[0Kbenchmarked parseText/medium array
time                 7.629 ms   (7.499 ms .. 7.744 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 7.458 ms   (7.395 ms .. 7.542 ms)
std dev              208.3 μs   (171.9 μs .. 277.0 μs)

[0Kbenchmarked generateText/medium array
time                 17.24 ms   (15.96 ms .. 18.39 ms)
                     0.977 R²   (0.964 R² .. 0.988 R²)
mean                 14.53 ms   (14.08 ms .. 15.18 ms)
std dev              1.386 ms   (1.051 ms .. 1.752 ms)
variance introduced by outliers: 44% (moderately inflated)

[0Kbenchmarked solveText/array[1000]
time                 21.31 ms   (20.49 ms .. 22.03 ms)
                     0.995 R²   (0.991 R² .. 0.997 R²)
mean                 22.93 ms   (22.41 ms .. 23.57 ms)
std dev              1.351 ms   (992.5 μs .. 1.704 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking solveText/array[1000] no sig ... took 9.449 s, total 56 iterations
[0Kbenchmarked solveText/array[1000] no sig
time                 161.5 ms   (148.9 ms .. 175.5 ms)
                     0.993 R²   (0.988 R² .. 0.998 R²)
mean                 173.9 ms   (168.9 ms .. 177.8 ms)
std dev              7.729 ms   (5.320 ms .. 11.07 ms)

Benchmark inflex-lang-time: FINISH
Success! Waiting for next file change.
Type help for available commands. Press enter to force a rebuild.

-}

main :: IO ()
main = do
  let !array1000 = T.concat ["[", T.intercalate "," (replicate 1000 "1"), "]"]
      !array2000 = T.concat ["[", T.intercalate "," (replicate 2000 "1"), "]"]
      !array4000 = T.concat ["[", T.intercalate "," (replicate 4000 "1"), "]"]
  when
    True
    (defaultMain
       [ bgroup
           "parseText"
           [ bench "array[1000]" (nf parseTextUpToErrorSuccess array1000)
           , bench "array[2000]" (nf parseTextUpToErrorSuccess array2000)
           , bench "array[4000]" (nf parseTextUpToErrorSuccess array4000)
           ]
       , bgroup
           "generateText"
           [ bench "array[1000]" (nf generateTextUpToErrorSuccess array1000)
           , bench "array[2000]" (nf generateTextUpToErrorSuccess array2000)
           , bench "array[4000]" (nf generateTextUpToErrorSuccess array4000)
           ]
       , bgroup
           "solveText"
           [ bench
             ("array[" <> show n <> "] SIG")
             (nfIO
                (do ref <- newSomeRef 0
                    binds <- newSomeRef mempty
                    RIO.runRIO
                      (SolveReader {glogfunc = mempty, counter = ref, binds})
                      (solveTextUpToErrorSuccess array)))
           | (n, array) <- [(1000 :: Int, array1000)]
           ]
       ])

parseTextUpToErrorSuccess :: Text -> Either () ()
parseTextUpToErrorSuccess = first (const ()) . second (const ()) . parseText ""

solveTextUpToErrorSuccess :: Text -> RIO SolveReader (Either () ())
solveTextUpToErrorSuccess = fmap (bimap (const ()) (const ())) . solveText mempty ""

generateTextUpToErrorSuccess :: Text -> Either () ()
generateTextUpToErrorSuccess = bimap (const ()) (const ())  . generateText mempty ""
