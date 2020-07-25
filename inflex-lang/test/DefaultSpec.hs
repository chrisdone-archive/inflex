{-# LANGUAGE OverloadedStrings #-}

-- | Check defaulting works as expected, and fails as expected.

module DefaultSpec where

import Inflex.Defaulter
import Test.Hspec

spec :: Spec
spec = it "1" (shouldBe (defaultText mempty "" "0") (Left (DefaulterError DO)))
