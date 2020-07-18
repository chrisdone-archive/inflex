{-# LANGUAGE OverloadedStrings #-}
-- |

module StepSpec where

import           Data.Text (Text)
import           Inflex.Display ()
import           Inflex.Stepper
import           RIO (textDisplay)
import           Test.Hspec

stepTextly :: Text -> Either ResolveStepError [Text]
stepTextly text = fmap (fmap textDisplay) (stepText mempty mempty "" text)

spec :: SpecWith ()
spec =
  describe
    "Single expressions"
    (do it "1" (shouldBe (stepTextly "1 :: Integer") (Right ["1"]))
        )
