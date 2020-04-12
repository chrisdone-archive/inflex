{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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

module Inflex.Server.Handlers.Forms where

import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Forge.Internal.Types as Forge
import           Inflex.Server.Forge
import           Lucid
import           Text.Email.Validate as Email

data RegisterSubmit = RegisterSubmit
  { registerEmail :: !EmailAddress
  , registerPassword :: !Text
  }

registerForm :: Forge.Form 'Forge.Unverified Identity (HtmlT m ()) (Field m) error RegisterSubmit
registerForm =
  RegisterSubmit <$>
  Forge.FieldForm (Forge.StaticFieldName "email") (EmailField Nothing) <*>
  Forge.FieldForm (Forge.StaticFieldName "password") (PasswordField Nothing)
