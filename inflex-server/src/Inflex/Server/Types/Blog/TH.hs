{-# LANGUAGE TemplateHaskell #-}

-- | Blog generation.

module Inflex.Server.Types.Blog.TH where

import Control.Monad.IO.Class
import Data.Char
import Data.FileEmbed.Stack
import Data.Maybe
import Language.Haskell.TH
import Sendfile
import System.Directory

-- | This reads all the files in the directory and generates a set of
-- name<->content tuples.
generateBlog :: Q [(FilePath,Exp)]
generateBlog = do
  blogDir <- wrapStackRoot root
  fps <-
    fmap (filter (not . all (== '.'))) (liftIO (getDirectoryContents blogDir))
  traverse
    (\fp -> do
       contents <- openFileFrom (root ++ "/" ++ fp)
       pure (fp, contents))
    fps
  where root = "inflex-content/blog"

mkBlogDataType :: [(FilePath,Exp)] -> Q [Dec]
mkBlogDataType names =
  fmap pure (dataD (pure []) (mkName "BlogEntryName") [] Nothing conses [])
  where
    conses = map (\(fp, _) -> normalC (mkName (toCons fp)) []) names

toCons :: [Char] -> [Char]
toCons =
  (blogPrefix <>) .
  mapMaybe
    (\c ->
       if isAlphaNum c
         then pure c
         else if c == '-'
                then pure '_'
                else Nothing) .
  reverse . drop 1 . dropWhile (/= '.') . reverse

blogPrefix :: [Char]
blogPrefix = "Blog_"
