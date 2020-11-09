{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Blog generation.

module Inflex.Server.Types.Blog where

import           Data.Maybe
import qualified Data.Text as T
import           Inflex.Server.Types.Blog.TH
import           Inflex.Server.Types.Article
import           Language.Haskell.TH
import           Text.Read
import           Yesod (PathPiece(..))

$(do files <- generateBlog
     (dt, nameAndGetters) <- mkBlogDataType files
     getter <-
            [d|
               getArticleByEntryName =
                 $(pure (LamCaseE (map (\(name,getter') ->
                                          Match (ConP name []) (NormalB getter') [])
                                       nameAndGetters))) |]
     pure (dt <> getter))

getArticleByEntryName :: BlogEntryName -> IO Article

deriving instance Show BlogEntryName
deriving instance Eq BlogEntryName
deriving instance Read BlogEntryName
deriving instance Enum BlogEntryName
deriving instance Bounded BlogEntryName

-- This code is sensitive, don't mess with it.
instance PathPiece BlogEntryName where
  toPathPiece =
    T.replace "_" "-" .
    (\t -> fromMaybe t (T.stripPrefix (T.pack blogPrefix) t)) . T.pack . show
  fromPathPiece = readMaybe . (blogPrefix <>) . T.unpack . T.replace "-" "_"
