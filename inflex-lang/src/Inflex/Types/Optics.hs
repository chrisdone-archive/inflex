{-# LANGUAGE TemplateHaskell #-}

-- |

module Inflex.Types.Optics where

import Inflex.Optics
import Inflex.Types
import Optics

$(do fmap
       concat
       (traverse
          (makeLensesWith (inflexRules []))
          [ ''Apply
          , ''Infix
          , ''Record
          , ''Prop
          , ''Array
          , ''Variant
          , ''If
          , ''Case
          , ''Alternative
          , ''FieldE
          ]))
$(makePrisms ''Expression)
