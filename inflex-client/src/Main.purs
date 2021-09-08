-- | Main entry point to the app UI.
--
-- Notes
--
-- Initial load:
--
-- Load the initial document JSON from body data attribute? Apparently
-- there are no limits on length
-- <https://stackoverflow.com/a/1496165/89574> but ideally it would be
-- small anyway, with lazy loading of heavier artifacts later.
--
-- This way we have one single request to the server as a dependency
-- for page paint. If latency is 9ms then we wait 9ms, not 9x2.
--
-- Above is small fry: it takes 16ms-25ms to render a simple DOM after
-- compiling the JS, which itself takes 18ms. Pre-rendering with
-- hydration would get better results. Server-side rendering via
-- nodejs? Possibly... rather than on-demand, as a cache that is
-- updated async? <https://github.com/purescript-halogen/purescript-halogen/issues/587#issuecomment-449952354>

module Main where

import Effect.Class (liftEffect)
import Prelude

import Effect (Effect)
import Inflex.Components.Doc as Doc
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

--------------------------------------------------------------------------------
-- Main entry point

main :: Effect Unit
main =
  HA.runHalogenAff
    (do body <- HA.awaitBody
        liftEffect swap
        runUI Doc.component unit body)

foreign import swap :: Effect Unit
