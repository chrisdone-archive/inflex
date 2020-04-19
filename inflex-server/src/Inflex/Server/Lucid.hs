{-# LANGUAGE OverloadedStrings #-}

-- |

module Inflex.Server.Lucid where

import Control.Monad.Reader
import Data.String
import Inflex.Server.App
import Lucid
import Yesod.Lucid

redirect_ :: Int -> Route App -> Lucid App ()
redirect_ i route =
  do url <- ask
     meta_ [httpEquiv_ "refresh", content_ (fromString (show i) <> ";url=" <> url route)]
