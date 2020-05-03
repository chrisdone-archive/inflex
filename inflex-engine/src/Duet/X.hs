-- |

module Duet.X where

import Control.Monad.ST
import Data.Dynamic
import Data.Mutable
import Data.STRef
import GHC.Exts (Any)

data Ref c a = Ref Int

data Container s c = Container { counter :: STRef s Integer, elements :: BRef s Any}

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

addRef :: a -> Container s c -> ST s (Ref c a)
addRef a (Container{counter,elements}) = do idx <- readSTRef counter
                                            modifySTRef' counter (+1)
                                            pushBack elements a
                                            pure (Ref idx)
