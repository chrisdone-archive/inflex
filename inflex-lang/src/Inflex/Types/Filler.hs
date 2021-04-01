{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-- |

module Inflex.Types.Filler where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Validation
import           Inflex.Types

data FillerError e
  = MissingGlobal (FillerEnv e)
                  Text
  | MissingGlobalUuid (FillerEnv e)
                      Uuid
  | OtherCellError Text
                   e
  | OtherCellUuidError Uuid
                       e
  deriving (Eq, Show)

newtype Filler e a = Filler
  { runFiller :: Validation (NonEmpty (FillerError e)) a
  } deriving (Functor, Applicative)

data FillerEnv e = FillerEnv
  { namesTohash :: !(Map Text (Either e Hash))
  , uuidsToHash :: !(Map Uuid (Either e Hash))
  } deriving (Show, Eq)

emptyFillerEnv :: FillerEnv e
emptyFillerEnv = FillerEnv {namesTohash = mempty, uuidsToHash = mempty}

insertNameAndUuid :: Text -> Uuid -> Either e Hash -> FillerEnv e -> FillerEnv e
insertNameAndUuid name uuid result FillerEnv {namesTohash, uuidsToHash} =
  FillerEnv
    { namesTohash = M.insert name result namesTohash
    , uuidsToHash = M.insert uuid result uuidsToHash
    }
