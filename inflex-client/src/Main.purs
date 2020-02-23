module Main where

import Prelude

import Effect (Effect)
import Inflex.App as App
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff
    (do body <- HA.awaitBody
        runUI App.component unit body)
