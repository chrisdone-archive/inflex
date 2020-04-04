{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}

-- | Make a file path relative to stack root.

module Stack
  ( makeRelativeToProject
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath

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
