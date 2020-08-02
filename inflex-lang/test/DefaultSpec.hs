{-# LANGUAGE OverloadedStrings #-}

-- | Check defaulting works as expected, and fails as expected.

module DefaultSpec where

import Inflex.Defaulter
import Test.Hspec

spec :: Spec
spec = do
  it "1" (shouldBe (defaultText mempty "" "0") (Left (DefaulterError DO)))
  it "1.0" (shouldBe (defaultText mempty "" "1.0") (Left (DefaulterError DO)))
  it
    "1 + 1.0"
    (shouldBe (defaultText mempty "" "1 + 1.0") (Left (DefaulterError DO)))
  it
    "\\x -> x + x"
    (shouldBe (defaultText mempty "" "\\x -> x + x") (Left (DefaulterError DO)))
