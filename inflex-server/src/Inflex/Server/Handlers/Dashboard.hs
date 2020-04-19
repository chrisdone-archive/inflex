{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Dashboard
  ( postAppDashboardR
  , getAppDashboardR
  ) where

import Data.Foldable
import Inflex.Server.App
import Inflex.Server.Session
import Inflex.Server.Types
import Lucid
import Yesod hiding (Html, toHtml)
import Yesod.Lucid

getAppDashboardR :: Handler (Html ())
getAppDashboardR =
  withLogin
    (\_ LoginState {loginAccountId} -> do
       documents <-
         runDB
           (selectList
              [DocumentAccount ==. fromAccountID loginAccountId]
              [Desc DocumentCreated])
       htmlWithUrl
         (do h1_ "Dashboard"
             p_ (a_ [href_ ""] "New Document") -- TODO:
             ul_
               (forM_
                  documents
                  (\(Entity documentId Document {..}) ->
                     li_ (toHtml documentName)))))

postAppDashboardR :: Handler (Html ())
postAppDashboardR = pure (pure ())
