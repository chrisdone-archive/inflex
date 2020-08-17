{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [_, inputf, outputf] -> do
      text <- T.readFile inputf
      let ls = T.lines text
          content = T.unlines (drop 1 ls)
      T.writeFile outputf "module Inflex.Shared.Spec where"
      hs <- T.readFile "templates/Shared.hs"
      T.writeFile "../inflex-server/src/Inflex/Shared.hs" (T.replace "$types" content hs)
      purs <- T.readFile "templates/Shared.purs"
      T.writeFile "../inflex-client/src/Inflex/Shared.purs" (T.replace "$types" content purs)
    _ -> error "bad arguments"
