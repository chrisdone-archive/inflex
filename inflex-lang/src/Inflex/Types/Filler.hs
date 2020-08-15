{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- |

module Inflex.Types.Filler where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Data.Validation
import           Inflex.Types

data FillerError =
  MissingGlobal (Map Text Hash)
                Text
  deriving (Eq, Show)

newtype Filler a = Filler { runFiller :: Validation (NonEmpty FillerError) a }
  deriving (Functor, Applicative)
