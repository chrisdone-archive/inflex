-- |

module Buffering where

import System.IO
setAppBuffering :: IO ()
setAppBuffering = hSetBuffering stdout NoBuffering
