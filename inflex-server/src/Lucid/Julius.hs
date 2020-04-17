-- | Lucid integration with Julius.

module Lucid.Julius where

import           Control.Monad.Reader
import qualified Data.Text.Lazy as LT
import           Lucid
import           Text.Julius
import           Yesod
import           Yesod.Lucid

julius_ :: JavascriptUrl (Route app) -> Lucid app ()
julius_ javascript = do
  url <- ask
  script_
    (LT.toStrict
       (renderJavascriptUrl
          (\route _attrs -> url route)
          javascript))
