-- | DEBUG

module Log (prettyWrite) where
import qualified Lexx
prettyWrite :: Show a => a -> IO ()
prettyWrite = Lexx.prettyWrite-- Limited 512
