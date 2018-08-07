module Main where

import Prelude (Unit, (<>))
import Effect.Console (log)
import Effect (Effect)

greet :: String -> String
greet name = "Hello, " <> name <> "!"

main :: Effect Unit
main = log (greet "World")
