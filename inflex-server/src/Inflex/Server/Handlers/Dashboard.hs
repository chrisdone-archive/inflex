{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Inflex.Server.Handlers.Dashboard
  ( postAppDashboardR
  , getAppDashboardR
  , postNewDocumentR
  , postDeleteDocumentR
  , postRenameDocumentR
  ) where

import           Control.Monad.Reader
import           Data.String
import qualified Data.Text as T
import           Data.Time
import           Database.Persist.Sql
import qualified Formatting
import qualified Formatting.Time
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
                   h1_ "Documents"
                   p_
                     "Your work in Inflex is split up into documents. We've added some \
                      \example documents for you below. You can create a new document to \
                      \start from scratch by hitting New Document."
                   p_
                     (do "Don't forget to check out the "
                         a_
                           [ href_
                               "https://community.inflex.io/t/the-inflex-document/17"
                           ]
                           "community forum guide on working with documents.")
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
                                            , onsubmit_
                                                "return confirm('Do you really want to delete this document?');"
                                            ]
                                            (button_
                                               [ title_
                                                   "Delete this document permanently"
                                               ]
                                               "×"))
                                    form_
                                      [ action_
                                          (url (RenameDocumentR documentId))
                                      , method_ "post"
                                      , class_ "rename-document"
                                      ]
                                      (do input_
                                            [ name_ "name"
                                            , type_ "text"
                                            , value_
                                                (let DocumentSlug slug =
                                                       documentName
                                                  in slug)
                                            ]
                                          button_
                                            [title_ "Rename this document"]
                                            "Rename")
                                    p_
                                      [ class_ "document-date"
                                      , title_ (T.pack (show documentCreated))
                                      ]
                                      (toHtml
                                         (Formatting.format
                                            (Formatting.Time.diff True)
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
                     -- , documentContent = Shared.InputDocument1 {cells = mempty}
                     , documentCreated = now'
                     , documentUpdated = now'
                     , documentAccount = fromAccountID loginAccountId
                     }
               let slug =
                     DocumentSlug
                       ("document-" <> fromString (show (fromSqlKey key)))
               update key [DocumentName =. slug] -- TODO: Check uniqueness
               insert_
                 Revision
                   { revisionAccount = fromAccountID loginAccountId
                   , revisionDocument = key
                   , revisionCreated = now'
                   , revisionContent = Shared.InputDocument1 {cells = mempty}
                   , revisionActive = True
                   , revisionActivated = now'
                   }
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

postRenameDocumentR :: DocumentId -> Handler ()
postRenameDocumentR documentId = do
  mdocumentSlug <- fmap (>>= fromPathPiece) (lookupPostParam "name")
  case mdocumentSlug of
    Nothing -> do
      slug <- (lookupPostParam "name")
      error (show slug)
               {-error redirect AppDashboardR-}
    Just documentSlug ->
      withLogin
        (\_ LoginState {loginAccountId = AccountID accountId} -> do
           (runDB $ do
              mexisting <-
                selectFirst
                  [ DocumentName ==. documentSlug
                  , DocumentAccount ==. toSqlKey accountId
                  ]
                  []
              case mexisting of
                Just {} -> pure ()
                Nothing -> do
                  now <- liftIO getCurrentTime
                  updateWhere
                    [ DocumentId ==. documentId
                    , DocumentAccount ==. toSqlKey accountId
                    ]
                    [DocumentName =. documentSlug, DocumentUpdated =. now])

           redirect AppDashboardR)
