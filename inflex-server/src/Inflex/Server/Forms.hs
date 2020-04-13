{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
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

module Inflex.Server.Forms where

import           Data.Functor.Identity
import qualified Forge.Internal.Types as Forge
import           Inflex.Server.Forge
import           Inflex.Server.Types
import           Lucid

data RegisterSubmit = RegisterSubmit
  { registerEmail :: !Email
  , registerPassword :: !Password
  }

registerForm :: Forge.Form 'Forge.Unverified Identity (HtmlT m ()) (Field m) error RegisterSubmit
registerForm = do
  registerEmail <-
    Forge.FieldForm (Forge.StaticFieldName "email") (EmailField Nothing)
  registerPassword <-
    Forge.FieldForm (Forge.StaticFieldName "password") (PasswordField Nothing)
  pure RegisterSubmit {..}
