{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

-- |

module Yesod.Forge
  ( generateForm
  , Submission(..)
  ) where

import           Data.Bifunctor
import qualified Data.Map.Strict as M
import           Forge.Generate
import           Forge.Internal.Types
import           Forge.Verify
import           Network.Wai
import           Yesod

data Submission parse view error a
  = Submitted (parse (Generated view error a))
  | NotSubmitted view
  deriving (Functor)

-- TODO: Put an anti-CSRF token in here?

generateForm ::
     ( MonadHandler m
     , Monad parse
     , Monoid view
     , FormField view field error
     , FormError error
     )
  => VerifiedForm 'Unverified parse view field error a
  -> m (Submission parse view error a)
generateForm verifiedForm = do
  method <- fmap requestMethod waiRequest
  if | method == "POST" ->
       do (inputs, _files) <- runRequestBody
          let inputMap =
                (M.fromListWith
                   (<>)
                   (map (first Key . second (pure . TextInput)) inputs))
          liftIO (putStrLn "generateForm: POST request")
          pure (Submitted (generate inputMap verifiedForm))
     | method == "GET" ->
       do liftIO (putStrLn "generateForm: GET request")
          pure (NotSubmitted (view verifiedForm))
     | otherwise -> invalidArgs []
