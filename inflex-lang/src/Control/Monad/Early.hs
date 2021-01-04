
module Control.Monad.Early where

import Control.Early
import Data.Foldable

{-# INLINE foldE #-}
foldE ::
     (Monad m, Foldable t, Applicative f, Early f)
  => (x -> a -> m (f x))
  -> x
  -> t a
  -> m (f x)
foldE cons nil = go nil . toList
  where
    go acc [] = pure (pure acc)
    go acc (x:xs) = do
      early (cons acc x) (\k -> go k xs)
