{-# LANGUAGE TemplateHaskell #-}

-- | Write out the schema files.

module Inflex.Shared.Gen where

import Inflex.Shared.TH

$(generateSchema)
