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

data RegisterSubmit = RegisterSubmit
  { registerEmail :: !Email
  , registerPassword :: !Password
  , registerUsername :: !Username
  }

registerForm :: Form error RegisterSubmit
registerForm = do
  registerUsername <-
    labelled
      "Username"
      (Forge.FieldForm
         (Forge.StaticFieldName "username")
         (UsernameField Nothing))
  registerEmail <-
    labelled
      "Email"
      (Forge.FieldForm (Forge.StaticFieldName "email") (EmailField Nothing))
  registerPassword <-
    labelled
      "Password"
      (Forge.FieldForm
         (Forge.StaticFieldName "password")
         (PasswordField
            PasswordConfig
              { def = Nothing
              , required = Required
              , autocomplete = CompleteNewPassword
              }))
  pure RegisterSubmit {..}
