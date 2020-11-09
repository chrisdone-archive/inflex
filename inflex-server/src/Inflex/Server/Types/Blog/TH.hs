{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Blog generation.

module Inflex.Server.Types.Blog.TH where

import           Blogfile
import           Control.Monad.IO.Class
import           Data.Char
import           Data.FileEmbed.Stack
import           Data.Maybe
import           Data.Time.QQ ()
import           Language.Haskell.TH
import           Sendfile ()
import           System.Directory

-- | This reads all the files in the directory and generates a set of
-- name<->content tuples.
generateBlog :: Q [(FilePath,Exp)]
generateBlog = do
  blogDir <- wrapStackRoot root
  fps <-
    fmap (filter (not . all (== '.'))) (liftIO (getDirectoryContents blogDir))
  traverse
    (\fp -> do
       contents <- openBlogFileFrom (root ++ "/" ++ fp)
       pure (fp, contents))
    fps
  where root = "inflex-content/blog"

mkBlogDataType :: [(FilePath,Exp)] -> Q ([Dec], [(Name, Exp)])
mkBlogDataType names =
  do d <- dataD (pure []) (mkName "BlogEntryName") [] Nothing (map snd conses) []
     pure ([d], map fst conses)
  where
    conses = map (\(fp, getter) -> let nom = mkName (toCons fp)
                                   in
                                   ((nom, getter), normalC nom [])) names

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
