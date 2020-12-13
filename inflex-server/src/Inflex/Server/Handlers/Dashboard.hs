{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Dashboard
  ( postAppDashboardR
  , getAppDashboardR
  , postNewDocumentR
  , postDeleteDocumentR
  ) where

import           Control.Monad.Reader
import           Data.String
import qualified Data.Text as T
import           Data.Time
import           Database.Persist.Sql
import           Formatting
import           Formatting.Time
import           GA
import qualified Inflex.Schema as Shared
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.Shop
import           Lucid
import           RIO (glog)
import           Yesod hiding (Html, toHtml)
import           Yesod.Lucid

getAppDashboardR :: Handler (Html ())
getAppDashboardR =
  withLogin
    (\_ state@LoginState {loginAccountId} -> do
       submitGA
       documents <-
         runDB
           (selectList
              [DocumentAccount ==. fromAccountID loginAccountId]
              [Desc DocumentCreated])
       now' <- liftIO getCurrentTime
       htmlWithUrl
         (shopTemplate
            (Registered state)
            (div_
               [class_ "dashboard"]
               (do url <- ask
                   form_
                     [action_ (url NewDocumentR), method_ "post"]
                     (button_ [class_ "new-document"] "New Document")
                   unless
                     (null documents)
                     (div_
                        [class_ "documents"]
                        (forM_
                           documents
                           (\(Entity documentId Document {..}) ->
                              div_
                                [class_ "document"]
                                (do div_
                                      [class_ "document-header"]
                                      (do p_
                                            [class_ "document-title"]
                                            (a_
                                               [ href_
                                                   (url
                                                      (AppEditorR documentName))
                                               ]
                                               (toHtml documentName))
                                          form_
                                            [ action_
                                                (url
                                                   (DeleteDocumentR documentId))
                                            , method_ "post"
                                            , class_ "delete-document"
                                            , onsubmit_ "return confirm('Do you really want to delete this document?');"
                                            ]
                                            (button_ [title_ "Delete this document permanently"] "×"))
                                    p_
                                      [ class_ "document-date"
                                      , title_ (T.pack (show documentCreated))
                                      ]
                                      (toHtml
                                         (format
                                            (diff True)
                                            (diffUTCTime documentCreated now')))))))))))

postAppDashboardR :: Handler (Html ())
postAppDashboardR = pure (pure ())

postNewDocumentR :: Handler ()
postNewDocumentR =
  withLogin
    (\_ LoginState {loginAccountId} -> do
       submitGA
       slug <-
         runDB
           (do now' <- liftIO getCurrentTime
               key <-
                 insert
                   Document
                     { documentName = DocumentSlug mempty
                     , documentContent = Shared.InputDocument1 {cells = mempty}
                     , documentCreated = now'
                     , documentUpdated = now'
                     , documentAccount = fromAccountID loginAccountId
                     }
               let slug =
                     DocumentSlug
                       ("document-" <> fromString (show (fromSqlKey key)))
               update key [DocumentName =. slug] -- TODO: Check uniqueness
               pure slug)
       glog CreateDocument
       redirect (AppEditorR slug))

postDeleteDocumentR :: DocumentId -> Handler ()
postDeleteDocumentR documentId =
  withLogin
    (\_ LoginState {loginAccountId=AccountID accountId} -> do
       runDB
         (deleteWhere
            [DocumentId ==. documentId, DocumentAccount ==. toSqlKey accountId])
       glog DeleteDocument
       redirect AppDashboardR)
