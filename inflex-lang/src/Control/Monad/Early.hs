
module Control.Monad.Early where

import Data.Foldable

foldEither ::
     (Monad m, Foldable t)
  => (x -> a -> m (Either e x))
  -> x
  -> t a
  -> m (Either e x)
foldEither cons nil = go nil . toList
  where
    go acc [] = pure (Right acc)
    go acc (x:xs) = do
      r <- cons acc x
      case r of
        Left e -> pure (Left e)
        Right k -> go k xs
