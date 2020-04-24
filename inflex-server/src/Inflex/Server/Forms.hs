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
import           Inflex.Server.Forge
import           Inflex.Server.Types

registerForm :: Forge.Default RegistrationDetails -> Form Error RegistrationDetails
registerForm mregistrationDetails = do
  registerUsername <-
    formGroup
      "Username"
      (Forge.FieldForm
         (Forge.StaticFieldName "username")
         Forge.RequiredField
         (fmap registerUsername mregistrationDetails)
         UsernameField)
  registerEmail <-
    formGroup
      "Email"
      (Forge.FieldForm
         (Forge.StaticFieldName "email")
         Forge.RequiredField
         (fmap registerEmail mregistrationDetails)
         (EmailField CompleteEmail))
  registerPassword <-
    formGroup
      "Password"
      (Forge.FieldForm
         (Forge.StaticFieldName "password")
         Forge.RequiredField
         (fmap registerPassword mregistrationDetails)
         (PasswordField PasswordConfig {autocomplete = CompleteNewPassword}))
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
