{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Dashboard
  ( postAppDashboardR
  , getAppDashboardR
  , postNewDocumentR
  ) where

import Control.Monad.Reader
import Data.Foldable
import Data.String
import Data.Time
import Database.Persist.Sql
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
             url <- ask
             form_
               [action_ (url NewDocumentR), method_ "post"]
               (button_ "New Document")
             ul_
               (forM_
                  documents
                  (\(Entity _documentId Document {..}) ->
                     li_
                       (a_
                          [href_ (url (AppEditorR documentName))]
                          (toHtml documentName))))))

postAppDashboardR :: Handler (Html ())
postAppDashboardR = pure (pure ())

postNewDocumentR :: Handler ()
postNewDocumentR =
  withLogin
    (\_ LoginState {loginAccountId} -> do
       slug <-
         runDB
           (do now <- liftIO getCurrentTime
               key <-
                 insert
                   Document
                     { documentName = DocumentSlug mempty
                     , documentContent = DocumentDecs mempty
                     , documentCreated = now
                     , documentAccount = fromAccountID loginAccountId
                     }
               let slug =
                     DocumentSlug
                       ("document-" <> fromString (show (fromSqlKey key)))
               update key [DocumentName =. slug] -- TODO: Check uniqueness
               pure slug)
       redirect (AppEditorR slug))
