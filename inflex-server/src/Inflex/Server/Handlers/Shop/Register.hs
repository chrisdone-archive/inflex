{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Inflex.Server.Handlers.Shop.Register
  ( handleShopRegisterR
  ) where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Validation
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Server.App
import           Inflex.Server.Forge
import           Inflex.Server.Forms
import           Inflex.Server.View.Shop
import           Lucid
import           Yesod hiding (Html, Field, toHtml)
import           Yesod.Forge
import           Yesod.Lucid

handleShopRegisterR :: Handler (Html ())
handleShopRegisterR = do
  submission <- generateForm verifiedRegisterForm
  case submission of
    NotSubmitted view -> htmlWithUrl (registerView view)
    Submitted parse -> do
      let Forge.Generated {generatedView = view, generatedValue = result} =
            runIdentity parse
      case result of
        Failure _errors -> htmlWithUrl (registerView view)
        Success RegisterSubmit {..} -> do
          accountId <-
            runDB
              (insert
                 Account
                   { accountEmail = registerEmail
                   , accountPassword = registerPassword
                   , accountUsername = registerUsername
                   })
          htmlWithUrl
            (shopTemplate
               (p_
                  (do "Created: "
                      toHtml (show accountId))))

registerView :: Lucid App () -> Lucid App ()
registerView formView =
  shopTemplate
    (do url <- ask
        h1_ "Register"
        form_
          [action_ (url ShopRegisterR), method_ "POST", novalidate_ ""]
          (do formView
              p_ (button_ "Continue")))

verifiedRegisterForm :: VerifiedForm Error RegisterSubmit
verifiedRegisterForm = $$($$(Forge.verify [||registerForm||]))
