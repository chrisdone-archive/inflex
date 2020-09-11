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
     pure ()

refresh_ :: Int -> Lucid App ()
refresh_ i = do
  meta_ [httpEquiv_ "refresh", content_ (fromString (show i))]
  pure ()

spinner_ :: Lucid App ()
spinner_ =
  div_
    [class_ "spinner-border text-primary", role_ "status"]
    (span_ [class_ "sr-only"] "Redirecting...")

containedColumn_:: Lucid App () -> Lucid App ()
containedColumn_ inner =
  div_
    [class_ "container-fluid"]
    (div_ [class_ "row"] (div_ [class_ "col"] inner))
