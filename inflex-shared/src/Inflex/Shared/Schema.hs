{-# LANGUAGE TemplateHaskell #-}

-- | Write out the schema files.

module Inflex.Shared.Schema where

import Inflex.Shared.TH

$(generateSchema)