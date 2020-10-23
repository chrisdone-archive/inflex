{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | Make a file path relative to stack root.

module Data.FileEmbed.Stack
  ( wrapStackRoot
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import Text.RawString.QQ
import System.FilePath

wrapStackRoot :: FilePath -> Q FilePath
#ifdef STACK_ROOT
wrapStackRoot fp = pure ([r|STACK_ROOT|] <> "/" <> fp)
#else
wrapStackRoot fp = makeRelativeToProject fp
  where _ = [r|...|]
#endif

makeRelativeToProject :: FilePath -> Q FilePath
makeRelativeToProject rel = do
    loc <- qLocation
    runIO $ do
        srcFP <- canonicalizePath $ loc_filename loc
        mdir <- findProjectDir srcFP
        case mdir of
            Nothing -> error $ "Could not find stack.yaml file for path: " ++ srcFP
            Just dir -> return $ dir </> rel
  where
    findProjectDir x = do
        let dir = takeDirectory x
        if dir == x
            then return Nothing
            else do
                contents <- getDirectoryContents dir
                if any isCabalFile contents
                    then return (Just dir)
                    else findProjectDir dir

    isCabalFile fp = takeFileName fp == "stack.yaml"
