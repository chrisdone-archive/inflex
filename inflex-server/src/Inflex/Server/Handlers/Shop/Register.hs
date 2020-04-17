{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
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
  , getCheckoutCreateR
  , getCheckoutCancelR
  , getCheckoutSuccessR
  ) where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Text (Text)
import           Data.Validation
import qualified Forge.Internal.Types as Forge
import qualified Forge.Verify as Forge
import           Inflex.Server.App
import           Inflex.Server.Forge
import           Inflex.Server.Forms
import           Inflex.Server.Types
import           Inflex.Server.View.Shop
import           Lucid
import           Lucid.Julius
import           Stripe
import           Yesod hiding (Html, Field, toHtml)
import           Yesod.Forge
import           Yesod.Lucid

--------------------------------------------------------------------------------
-- Registration form

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
          _accountId <-
            runDB
              (insert
                 Account
                   { accountEmail = registerEmail
                   , accountPassword = registerPassword
                   , accountUsername = registerUsername
                   })
          redirect CheckoutCreateR

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

--------------------------------------------------------------------------------
-- Checkout create page

getCheckoutCreateR :: Handler (Html ())
getCheckoutCreateR = do
  render <- getUrlRender
  Config {stripeConfig} <- fmap appConfig getYesod
  result <-
    createSession
      StripeSession
        { stripeConfig
        , successUrl = render CheckoutSuccessR
        , cancelUrl = render CheckoutCancelR
        }
  case result of
    Left err -> error (show err) -- TODO:
    Right CreateSessionResponse {checkoutSessionId} ->
      htmlWithUrl
        (html_
           (do head_ (script_ [src_ "https://js.stripe.com/v3/"] ("" :: Text))
               body_
                 (do h1_ "One moment"
                     noscript_
                       "Please enable JavaScript so that we can secrely send you to Stripe."
                     p_ "Redirecting you to Stripe ..."
                     julius_
                       (stripeJavaScript
                          (publishableApiKey stripeConfig)
                          checkoutSessionId))))

stripeJavaScript :: PublishableApiKey -> CheckoutSessionId -> JavascriptUrl (Route App)
stripeJavaScript stripePublishableKey checkoutSessionId = [julius|
var stripe = Stripe(#{stripePublishableKey});
setTimeout (function () {
stripe.redirectToCheckout({
  sessionId: #{checkoutSessionId}
}).then(function (result) {
  // If `redirectToCheckout` fails due to a browser or network
  // error, display the localized error message to your customer
  // using `result.error.message`.
  console.log(result);
});
}, 1000);
|]

--------------------------------------------------------------------------------
-- Checkout cancel page

getCheckoutCancelR :: Handler (Html ())
getCheckoutCancelR =
  htmlWithUrl
    (do h1_ "Checkout cancelled"
        p_ "Cancelled!")

--------------------------------------------------------------------------------
-- Checkout success page

getCheckoutSuccessR :: Handler (Html ())
getCheckoutSuccessR =
  htmlWithUrl
    (do h1_ "Checkout succeeded!"
        p_ "Great success!"
        p_ "Now we go to the dashboard...")
