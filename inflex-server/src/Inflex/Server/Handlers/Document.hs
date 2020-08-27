{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- -- |

module Inflex.Server.Handlers.Document where
--   ( postAppRefreshR
--   , getAppCssR
--   , getAppJsR
--   , getAppEditorR
--   , postRefreshR
--   , getViewDocumentR
--   ) where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text (Text)
import           Database.Persist.Sql
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import           Inflex.Server.View.App
import qualified Inflex.Shared as Shared
import           Lucid
import           Sendfile
import           Shakespearean
import           Text.Lucius
import           Yesod hiding (Html)
import           Yesod.Lucid

-- expressionToEditor :: Expression Type Name l -> Editor
-- expressionToEditor =
--   \case
--     LiteralExpression unkindedType (IntegerLiteral integer) -> IntegerE integer
--     ArrayExpression unkindedType es -> ArrayE (fmap expressionToEditor es)
--     RowExpression unkindedType es ->
--       RowE
--         (M.fromList
--            (map
--               (first (\(Identifier i) -> T.pack i))
--               (M.toList (fmap expressionToEditor es))))
--     ApplicationExpression unkindedType (ConstructorExpression unk (ConstructorName _ name)) inner ->
--       ConsE (T.pack name) (expressionToEditor inner)
--     e -> MiscE (T.pack (printExpression defaultPrint e))

-- maxSteps :: Int
-- maxSteps = 100

-- postAppRefreshR :: DocumentId -> Handler Value
-- postAppRefreshR = refreshHandler

getAppEditorR :: DocumentSlug -> Handler (Html ())
getAppEditorR slug =
  withLogin
    (\_ state@(LoginState {loginAccountId}) -> do
       documentId <-
         do mdoc <-
              runDB
                (selectFirst
                   [ DocumentAccount ==. fromAccountID loginAccountId
                   , DocumentName ==. slug
                   ]
                   [])
            case mdoc of
              Nothing -> notFound
              Just (Entity documentId _) -> pure (documentId)
       htmlWithUrl
         (appTemplate
            (Registered state)
            ((do url <- ask
                 script_
                   [type_ "text/javascript"]
                   (do toHtmlRaw "window['inflexDocumentId'] = "
                       toHtmlRaw (encode documentId)
                       ";")
                 script_ [type_ "text/javascript", src_ (url AppJsR)] ""))))


getAppJsR :: Handler TypedContent
getAppJsR = $(sendFileFrom "application/javascript" "inflex-client/app.js")

getAppCssR :: Handler Css
getAppCssR = $(luciusFileFrom "inflex-server/templates/app.lucius")

postAppRpcR :: Text -> Handler TypedContent
postAppRpcR name = selectRep (provideRep (rpcHandler name))

--------------------------------------------------------------------------------
-- Refresh handler

rpcHandler :: Text -> Handler Value
rpcHandler name =
  case name of
    "loadDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcLoadDocument input
      pure (toJSON output)
    _ -> error "Invalid RPC function."

rpcLoadDocument :: Shared.DocumentId -> Handler Shared.OutputDocument
rpcLoadDocument docId =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       mdoc <-
         runDB
           (selectFirst
              [ DocumentAccount ==. fromAccountID loginAccountId
              , DocumentId ==. toSqlKey (fromIntegral docId)
              ]
              [])
       case mdoc of
         Nothing -> notFound
         Just (Entity _ Document {documentContent = document}) ->
           pure (Shared.OutputDocument mempty) -- TODO: Fill it.
     )
