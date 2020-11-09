{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Inflex.Server.Types.Article where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (Day)
import           Data.Time.QQ ()
import           Language.Haskell.TH.Lift (Lift)
import           Sendfile ()

data Article = Article
  { title :: Text
  , date :: Day
  , content :: Text
  } deriving (Read, Show, Lift)

parseArticle :: ByteString -> Maybe Article
parseArticle s =
  case reads (T.unpack (T.decodeUtf8 s)) of
    [(article, content')] ->
      pure (article {content = T.strip (T.pack content')})
    _ -> Nothing
