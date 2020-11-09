{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Server.Handlers.Blog where

import Control.Monad.Reader
import Data.Foldable
import Data.String
import Inflex.Server.App
import Inflex.Server.Types
import Inflex.Server.Types.Blog
import Lucid
import Yesod hiding (Html)
import Yesod.Lucid

getBlogEntryR :: BlogEntryName -> Handler (Html ())
getBlogEntryR entryName = htmlWithUrl (p_ (Lucid.toHtml (fromString (show entryName))))

getBlogR :: Handler (Html ())
getBlogR =
  htmlWithUrl
    (do h1_ "Blog archive"
        url <- ask
        ul_
          (traverse_
             (\(blogEntryName :: BlogEntryName) ->
                li_
                  (a_
                     [href_ (url (BlogEntryR blogEntryName))]
                     (Lucid.toHtml (show blogEntryName))))
             [minBound .. maxBound]))
