{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- |

module Inflex.Types.Filler where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Data.Validation
import           Inflex.Types

data FillerError e
  = MissingGlobal (Map Text (Either e Hash))
                  Text
  | OtherCellError Text e
  deriving (Eq, Show)

newtype Filler e a = Filler
  { runFiller :: Validation (NonEmpty (FillerError e)) a
  } deriving (Functor, Applicative)
