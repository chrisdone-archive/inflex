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

-- |

module Inflex.Server.Handlers.Document
  ( postAppRefreshR
  , getAppCssR
  , getAppJsR
  , getAppEditorR
  , postRefreshR
  , getViewDocumentR
  ) where

import                 Control.Monad.Catch (SomeException)
import "monad-logger"  Control.Monad.Logger
import                 Control.Monad.Reader
import "inflex-engine" Control.Monad.Supply
import                 Control.Monad.Writer
import                 Data.Aeson
import                 Data.Bifunctor
import                 Data.Foldable
import                 Data.HashMap.Strict (HashMap)
import qualified       Data.HashMap.Strict as HM
import qualified       Data.Map.Strict as M
import                 Data.Maybe
import                 Data.Semigroup ((<>))
import                 Data.Text (Text)
import qualified       Data.Text as T
import qualified       Data.UUID as UUID
import qualified       Data.UUID.V4 as V4
import qualified       Data.Vector as V
import                 Duet.Infer
import                 Duet.Parser
import                 Duet.Printer
import                 Duet.Simple
import                 Duet.Tokenizer
import                 Duet.Types
import                 Inflex.Server.App
import                 Inflex.Server.Session
import                 Inflex.Server.Types
import                 Inflex.Server.View.App
import                 Lucid
import                 Sendfile
import                 Shakespearean
import                 Text.Lucius
import                 Yesod hiding (Html)
import                 Yesod.Lucid

expressionToEditor :: Expression Type Name l -> Editor
expressionToEditor =
  \case
    LiteralExpression unkindedType (IntegerLiteral integer) -> IntegerE integer
    ArrayExpression unkindedType es -> ArrayE (fmap expressionToEditor es)
    RowExpression unkindedType es ->
      RowE
        (M.fromList
           (map
              (first (\(Identifier i) -> T.pack i))
              (M.toList (fmap expressionToEditor es))))
    ApplicationExpression unkindedType (ConstructorExpression unk (ConstructorName _ name)) inner ->
      ConsE (T.pack name) (expressionToEditor inner)
    e -> MiscE (T.pack (printExpression defaultPrint e))

maxSteps :: Int
maxSteps = 100

postAppRefreshR :: DocumentId -> Handler Value
postAppRefreshR = refreshHandler

getAppEditorR :: DocumentSlug -> Handler (Html ())
getAppEditorR slug =
  withLogin
    (\_ state@(LoginState {loginAccountId}) -> do
       (documentId, initialDecs') <-
         do mdoc <-
              runDB
                (selectFirst
                   [ DocumentAccount ==. fromAccountID loginAccountId
                   , DocumentName ==. slug
                   ]
                   [])
            case mdoc of
              Nothing -> notFound
              Just (Entity documentId Document { documentContent = DocumentDecs decs
                                               , ..
                                               }) ->
                liftIO
                  (do decs' <-
                        fmap
                          (HM.fromList . toList)
                          (traverse
                             (\dec -> do
                                uuid <- V4.nextRandom
                                pure (UUID.toText uuid, dec))
                             decs)
                      fmap (documentId, ) (evaluateInputDocument decs'))
       htmlWithUrl
         (appTemplate
            (Registered state)
            (do doctype_
                url <- ask
                html_
                  (do head_
                        (do link_ [rel_ "shortcut icon", href_ "#"]
                            title_ "InflexApp"
                            link_
                              [ rel_ "stylesheet"
                              , type_ "text/css"
                              , href_ (url AppCssR)
                              ])
                      body_
                        (do script_
                              [ src_
                                  "https://cdn.jsdelivr.net/npm/chart.js@2.9.3/dist/Chart.min.js"
                              , integrity_
                                  "sha256-R4pqcOYV8lt7snxMQO/HSbVCFRPMdrhAFMH+vr9giYI="
                              , crossorigin_ "anonymous"
                              ]
                              ""
                            script_
                              [type_ "text/javascript"]
                              (do toHtmlRaw "window['inflexDocument'] = "
                                  toHtmlRaw (encode initialDecs')
                                  ";"
                                  toHtmlRaw "window['inflexDocumentId'] = "
                                  toHtmlRaw (encode documentId)
                                  ";")
                            script_
                              [type_ "text/javascript", src_ (url AppJsR)]
                              "")))))

getAppJsR :: Handler TypedContent
getAppJsR = $(sendFileFrom "application/javascript" "inflex-client/app.js")

getAppCssR :: Handler Css
getAppCssR = $(luciusFileFrom "inflex-server/templates/app.lucius")

postRefreshR :: DocumentId -> Handler TypedContent
postRefreshR documentId = selectRep (provideRep (refreshHandler documentId))

--------------------------------------------------------------------------------
-- Refresh handler

refreshHandler :: DocumentId -> HandlerFor App Value
refreshHandler documentId =
  withLogin
    (\_ (LoginState {loginAccountId}) -> do
       mdoc <-
         runDB
           (selectFirst
              [ DocumentAccount ==. fromAccountID loginAccountId
              , DocumentId ==. documentId
              ]
              [])
       case mdoc of
         Nothing -> notFound
         Just (Entity documentId _) -> do
           inputDocument :: HashMap Text DecIn <- requireCheckJsonBody
           runDB (update documentId [DocumentContent =. DocumentDecs (V.fromList (HM.elems inputDocument))])
           evaluateInputDocument inputDocument)

evaluateInputDocument :: Monad m => HashMap Text DecIn -> m Value
evaluateInputDocument inputDocument = do
  let parsedDocument =
        map
          (\(uuid, decIn@DecIn {name, rhs}) ->
             (uuid, Identifier (T.unpack name), rhs, parseDecIn decIn))
          (HM.toList inputDocument)
      evaluatedDocument =
        case mapM
               (\(uuid, i, rhs, r) -> fmap (uuid, i, rhs, ) (fmap snd r))
               parsedDocument of
          Left {} ->
            map
              (\(uuid, Identifier name, rhs, result) ->
                 ( uuid
                 , DecOut
                     { name = T.pack name
                     , rhs
                     , result =
                         case result of
                           Left e -> Left (T.pack (show e))
                           Right v -> Left "... [waiting]"
                     }))
              parsedDocument
          Right r -> runProgram r
  pure (toJSON (Object (fmap toJSON (HM.fromList evaluatedDocument))))

--------------------------------------------------------------------------------
-- Duet helpers

-- TOOD: Deal with max steps, should throw an error.
runProgram ::
     [(Text, Identifier, Text, Expression UnkindedType Identifier Location)]
  -> [(Text, DecOut)]
runProgram decls = map toDecOut final
  where
    toDecOut (uuid, name, rhs, result) =
      ( uuid
      , DecOut
          { name
          , rhs
          , result =
              case result of
                Left ex -> Left (T.pack (show ex))
                Right results ->
                  case results of
                    [] -> Left "No result (didn't start!)" -- TODO: Handle properly.
                    xs ->
                      if length xs > maxSteps
                        then Left "No result (didn't finish!)"
                        else Right (expressionToEditor (last xs))
          })
    final =
      case overall of
        Right k -> k
        Left e ->
          map (\(t, Identifier i, rhs, _) -> (t, T.pack i, rhs, Left e)) decls
    overall =
      runNoLoggingT
        (evalSupplyT
           (do (binds, ctx) <-
                 createContext
                   (predefinedTypes <>
                    map
                      (\(i, e) ->
                         let loc = expressionLabel e
                          in BindDecl
                               loc
                               (ImplicitBinding
                                  (ImplicitlyTypedBinding
                                     loc
                                     (i, loc)
                                     [makeAlt loc e])))
                      (map (\(uuid, ident, _, expr) -> (ident, expr)) decls))
               idx <- peek
               pure
                 (map
                    (\(uuid, Identifier i, rhs, _) ->
                       ( uuid
                       , T.pack i
                       , rhs
                       , runNoLoggingT
                           (evalSupplyT
                              (execWriterT
                                 (runStepper
                                    (maxSteps + 1)
                                    ctx
                                    (fmap (fmap typeSignatureA) binds)
                                    i))
                              [idx ..])))
                    decls))
           [1 ..])

predefinedTypes :: [Decl UnkindedType Identifier Location]
predefinedTypes =
  case parseText "" "data BarChart a = BarChart a" of
    Left e -> []
    Right x -> x

--------------------------------------------------------------------------------
-- Parsing step

-- | Result of parsing the declaration.
data ParseResult
  = BadNameSyntax SomeException Text
  | BadExpressionSyntax Identifier SomeException Text
  deriving (Show)

-- | Parsing a declaration.
parseDecIn ::
     DecIn
  -> Either SomeException ( Identifier
                          , Expression UnkindedType Identifier Location)
parseDecIn DecIn {name, rhs} =
  case parseTextWith
         (consumeToken
            (\case
               Variable i -> pure i
               _ -> Nothing))
         (T.unpack name)
         name of
    Left e -> Left e
    Right (ident, _) ->
      case parseTextWith expParser (T.unpack name <> "'s expression") rhs of
        Left e -> Left e
        Right expr -> pure (Identifier (T.unpack ident), expr)

getViewDocumentR :: Username -> DocumentSlug -> Handler ()
getViewDocumentR _ _ = pure ()
