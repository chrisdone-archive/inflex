{-# LANGUAGE NamedFieldPuns #-}

-- | RPC handlers.

module Inflex.Server.Handlers.Rpc where

import           Database.Persist.Sql
import           Inflex.Server.App
import           Inflex.Server.Session
import           Inflex.Server.Types
import qualified Inflex.Schema as Schema
import           Yesod hiding (Html)

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

rpcRefreshDocument :: Schema.InputDocument -> Handler Schema.OutputDocument
rpcRefreshDocument = undefined
