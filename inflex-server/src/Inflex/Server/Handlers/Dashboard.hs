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
import Formatting
import Formatting.Time
import Inflex.Server.App
import Inflex.Server.Session
import Inflex.Server.Types
import Inflex.Server.View.Shop
import qualified Inflex.Shared as Shared
import Lucid
import Yesod hiding (Html, toHtml)
import Yesod.Lucid

getAppDashboardR :: Handler (Html ())
getAppDashboardR =
  withLogin
    (\_ state@LoginState {loginAccountId} -> do
       documents <-
         runDB
           (selectList
              [DocumentAccount ==. fromAccountID loginAccountId]
              [Desc DocumentCreated])
       now <- liftIO getCurrentTime
       htmlWithUrl
         (shopTemplate
            (Registered state)
            (do url <- ask
                div_
                  [class_ "container-fluid"]
                  (div_
                     [class_ "row"]
                     (div_
                        [class_ "col"]
                        (do h1_ "Dashboard"
                            form_
                              [action_ (url NewDocumentR), method_ "post"]
                              (button_ [class_ "btn-primary btn"] "New Document"))))
                unless
                  (null documents)
                  (div_
                     [class_ "container-fluid mt-3"]
                     (div_
                        [class_ "row"]
                        (div_
                           [class_ "col"]
                           (do div_
                                 [class_ "card-deck"]
                                 (forM_
                                    documents
                                    (\(Entity _documentId Document {..}) ->
                                       div_
                                         [class_ "card"]
                                         (do img_ []
                                             div_
                                               [class_ "card-body"]
                                               (do h5_
                                                     [class_ "card-title"]
                                                     (a_
                                                        [ href_ (url (AppEditorR documentName))
                                                        ]
                                                        (toHtml documentName))
                                                   p_
                                                     [class_ "card-text"]
                                                     "Example description here...")
                                             div_
                                               [class_ "card-footer"]
                                               (small_
                                                  [class_ "text-muted"]
                                                  (do "Created "
                                                      toHtml (format (diff True)
                                                                     (diffUTCTime documentCreated now))))))))))))))

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
                     , documentContent = Shared.InputDocument mempty
                     , documentCreated = now
                     , documentAccount = fromAccountID loginAccountId
                     }
               let slug =
                     DocumentSlug
                       ("document-" <> fromString (show (fromSqlKey key)))
               update key [DocumentName =. slug] -- TODO: Check uniqueness
               pure slug)
       redirect (AppEditorR slug))
