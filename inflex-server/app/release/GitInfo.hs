{-# LANGUAGE TemplateHaskell #-}

-- |

module GitInfo (gitHash) where

import GitHash

gitInfo :: GitInfo
gitInfo = $$(tGitInfoCwd)

gitHash :: String
gitHash = giHash gitInfo
