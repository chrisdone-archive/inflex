-- |

module Inflex.Optics where

import Data.Char
import Language.Haskell.TH.Syntax
import Optics

inflexRules :: [Name] -> LensRules
inflexRules names =
  set
    lensField
    (\tyname _ name ->
       [ (let Name (OccName tn) _ = tyname
              Name (OccName n) f = name
           in TopName (Name (OccName (downcaseFst tn <> upcaseFst n <> "L")) f))
           | elem name names
       ])
    lensRules
  where upcaseFst (x:xs) = toUpper x : xs
        upcaseFst xs = xs
        downcaseFst (x:xs) = toLower x : xs
        downcaseFst xs = xs
