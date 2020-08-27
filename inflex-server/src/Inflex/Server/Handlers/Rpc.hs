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

module Inflex.Server.Handlers.Rpc where
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
import qualified Inflex.Schema as Schema
import           Lucid
import           Sendfile
import           Shakespearean
import           Text.Lucius
import           Yesod hiding (Html)
import           Yesod.Lucid

postAppRpcR :: Text -> Handler TypedContent
postAppRpcR name = selectRep (provideRep (rpcHandler name))

rpcHandler :: Text -> Handler Value
rpcHandler name =
  case name of
    "loadDocument" -> do
      input <- requireCheckJsonBody
      output <- rpcLoadDocument input
      pure (toJSON output)
    _ -> error "Invalid RPC function."

rpcLoadDocument :: Schema.DocumentId -> Handler Schema.OutputDocument
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
           pure (Schema.OutputDocument mempty) -- TODO: Fill it.
     )
