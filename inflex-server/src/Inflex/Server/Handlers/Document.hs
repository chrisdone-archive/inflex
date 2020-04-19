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
  , getAppR
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
import                 Data.HashMap.Strict (HashMap)
import qualified       Data.HashMap.Strict as HM
import                 Data.Semigroup ((<>))
import                 Data.Text (Text)
import qualified       Data.Text as T
import qualified       Data.UUID as UUID
import qualified       Data.UUID.V4 as V4
import                 Data.Vector (Vector)
import                 Duet.Infer
import                 Duet.Parser
import                 Duet.Printer
import                 Duet.Simple
import                 Duet.Tokenizer
import                 Duet.Types
import                 Inflex.Server.App
import                 Inflex.Server.Types
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
    e  -> MiscE (T.pack (printExpression defaultPrint e))

maxSteps :: Int
maxSteps = 100

initialDecs :: [DecIn]
initialDecs =
  [ DecIn {name = "some_table", rhs = "[2 * 6, 6, 9, value1]"}
  , DecIn {name = "value1", rhs = "23 * 5"}
  ]

postAppRefreshR :: Handler Value
postAppRefreshR = refreshHandler

getAppR :: Handler (Html ())
getAppR = do
  initialDecs' <-
    liftIO
      (do decs <-
            fmap
              HM.fromList
              (traverse
                 (\dec -> do
                    uuid <- V4.nextRandom
                    pure (UUID.toText uuid, dec))
                 initialDecs)
          evaluateInputDocument decs)
  htmlWithUrl
    (do doctype_
        url <- ask
        html_
          (do head_
                (do link_ [rel_ "shortcut icon" ,href_ "#"]
                    title_ "InflexApp"
                    link_
                      [rel_ "stylesheet", type_ "text/css", href_ (url AppCssR)])
              body_
                (do script_
                      [type_ "text/javascript"]
                      (do toHtmlRaw "window['inflexDocument'] = "
                          toHtmlRaw (encode initialDecs')
                          ";")
                    script_ [type_ "text/javascript", src_ (url AppJsR)] "")))

getAppJsR :: Handler TypedContent
getAppJsR = $(sendFileFrom "application/javascript" "inflex-client/app.js")

getAppCssR :: Handler Css
getAppCssR = $(luciusFileFrom "inflex-server/templates/app.lucius")

getShopCssR :: Handler Css
getShopCssR = $(luciusFileFrom "inflex-server/templates/shop.lucius")

postRefreshR :: Handler TypedContent
postRefreshR = selectRep (provideRep refreshHandler)

--------------------------------------------------------------------------------
-- Refresh handler

refreshHandler :: HandlerFor App Value
refreshHandler = do
  inputDocument :: HashMap Text DecIn <- requireCheckJsonBody
  evaluateInputDocument inputDocument

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
                   (map
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

getAppEditorR :: DocumentSlug -> Handler (Html ())
getAppEditorR _ = getAppR

getViewDocumentR :: Username -> DocumentSlug -> Handler ()
getViewDocumentR _ _ = pure ()
