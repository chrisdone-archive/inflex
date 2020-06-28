{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
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

module Inflex.Server.Forms where

import qualified Forge.Internal.Types as Forge
import           Inflex.Server.App
import           Inflex.Server.Forge
import           Inflex.Server.Types
import           Yesod

data RegisterError
  = RegisterError Error
  | UsernameTaken
  | EmailTaken
  deriving (Show, Eq)

instance Forge.FormError RegisterError where
  missingInputError = RegisterError . Forge.missingInputError
  invalidInputFormat x y = RegisterError (Forge.invalidInputFormat x y)

instance ShowError RegisterError where
  showError =
    \case
      RegisterError e -> showError e
      UsernameTaken -> "That username is already in use, sorry!"
      EmailTaken -> "That email is already in use, sorry!" -- TODO: Add link to sign in.

registerForm :: Forge.Default RegistrationDetails -> Form RegisterError RegistrationDetails
registerForm mregistrationDetails = do
  registerUsername <-
    formGroup
      "Username"
      (Forge.ParseForm
         parseUniqueUsername
         (Forge.MapErrorForm
            RegisterError
            (Forge.FieldForm
               (Forge.StaticFieldName "username")
               Forge.RequiredField
               (fmap registerUsername mregistrationDetails)
               UsernameField)))
  registerEmail <-
    formGroup
      "Email"
      (Forge.ParseForm
         parseUniqueEmail
         (Forge.MapErrorForm
            RegisterError
            (Forge.FieldForm
               (Forge.StaticFieldName "email")
               Forge.RequiredField
               (fmap registerEmail mregistrationDetails)
               (EmailField CompleteEmail))))
  registerPassword <-
    formGroup
      "Password"
      (Forge.MapErrorForm
         RegisterError
         (Forge.FieldForm
            (Forge.StaticFieldName "password")
            Forge.RequiredField
            (fmap registerPassword mregistrationDetails)
            (PasswordField PasswordConfig {autocomplete = CompleteNewPassword})))
  pure RegistrationDetails {..}

loginForm :: Form Error (Email, Password)
loginForm = do
  loginEmail <-
    formGroup
      "Email"
      (Forge.FieldForm
         (Forge.StaticFieldName "email")
         Forge.RequiredField
         Forge.noDefault
         (EmailField CompleteEmail))
  loginPassword <-
    formGroup
      "Password"
      (Forge.FieldForm
         (Forge.StaticFieldName "password")
         Forge.RequiredField
         Forge.noDefault
         (PasswordField PasswordConfig {autocomplete = CurrentPassword}))
  pure (loginEmail, loginPassword)

parseUniqueUsername :: Username -> YesodDB App (Either RegisterError Username)
parseUniqueUsername username = do
  result <- selectFirst [AccountUsername ==. username] []
  case result of
    Nothing -> pure (pure username)
    Just {} -> pure (Left UsernameTaken)

parseUniqueEmail :: Email -> YesodDB App (Either RegisterError Email)
parseUniqueEmail email = do
  result <- selectFirst [AccountEmail ==. email] []
  case result of
    Nothing -> pure (pure email)
    Just {} -> pure (Left EmailTaken)
