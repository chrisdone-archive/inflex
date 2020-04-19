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

registerForm :: Forge.Default RegistrationDetails -> Form Error RegistrationDetails
registerForm mregistrationDetails = do
  registerUsername <-
    labelled
      "Username"
      (Forge.FieldForm
         (Forge.StaticFieldName "username")
         Forge.RequiredField
         Forge.noDefault
         UsernameField)
  registerEmail <-
    labelled
      "Email"
      (Forge.FieldForm
         (Forge.StaticFieldName "email")
         Forge.RequiredField
         Forge.noDefault
         EmailField)
  registerPassword <-
    labelled
      "Password"
      (Forge.FieldForm
         (Forge.StaticFieldName "password")
         Forge.RequiredField
         Forge.noDefault
         (PasswordField PasswordConfig {autocomplete = CompleteNewPassword}))
  pure RegistrationDetails {..}
