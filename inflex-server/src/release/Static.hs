-- |

module Static where

import Data.FileEmbed.Stack
import Language.Haskell.TH
import Yesod.EmbeddedStatic

makeStatic :: Q [Dec]
makeStatic =
  mkEmbeddedStatic
    False
    "app_static"
    [ wrapStackRoot "inflex-server/static" >>= embedDir
    , wrapStackRoot "inflex-client/app.js" >>= embedFileAt "inflex_client_app_js"
    ]
