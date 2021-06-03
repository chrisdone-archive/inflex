{-# LANGUAGE TemplateHaskellQuotes #-}

-- | A simple way to match a pattern.

module Match where

import Language.Haskell.TH (Q, Pat, Exp)
import qualified Language.Haskell.TH as TH

-- | Take a pattern and see whether it matches.
match :: Q Pat -> Q Exp
match pat =
  TH.lamCaseE
    [ TH.match pat (TH.normalB (TH.conE 'True)) []
    , TH.match TH.wildP (TH.normalB (TH.conE 'False)) []
    ]
