-- |

module Inflex.Optics where

import Data.Char
import Data.List
import Language.Haskell.TH.Syntax
import Optics

inflexRules :: [Name] -> LensRules
inflexRules names =
  set
    lensField
    (\tyname _ name ->
       [ (let Name (OccName tn) _ = tyname
           in TopName
                (Name
                   (OccName
                      (downcaseFst tn <> upcaseFst (nameBase' name) <> "L"))
                   NameS))
       | elem (nameBase' name) (map nameBase' names) || null names
       ])
    lensRules
  where
    upcaseFst (x:xs) = toUpper x : xs
    upcaseFst xs = xs
    downcaseFst (x:xs) = toLower x : xs
    downcaseFst xs = xs
    nameBase' =
      (\name -> maybe name (takeWhile (/= ':')) (stripPrefix "$sel:" name)) .
      nameBase
