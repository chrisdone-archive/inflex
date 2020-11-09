{-# LANGUAGE TemplateHaskell #-}

-- |

module Blogfile where

import qualified Data.ByteString as S
import           Data.FileEmbed.Stack
import           Data.Time.QQ ()
import           Inflex.Server.Types.Article
import           Language.Haskell.TH
import           Sendfile ()

openBlogFileFrom :: FilePath -> Q Exp
openBlogFileFrom fp0 = do
  fp <- wrapStackRoot fp0
  bs <- runIO (S.readFile fp)
  case parseArticle bs of
    Just article -> [|pure article|]
    Nothing -> error ("Error: Couldn't parse article: " ++ fp0)
