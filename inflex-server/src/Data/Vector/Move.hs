-- | Move elements in a vector.

module Data.Vector.Move where

import           Data.Ix
import qualified Data.List as List
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- | Move a set of elements N places. O(n^2)
moveElements :: Vector Int -> Int -> Vector a -> Vector a
moveElements origins places vec0 =
  List.foldl'
    (\vec origin -> moveElement origin places vec)
    vec0
    (List.sortBy
       -- If we're going backwards, then apply each origin from the
       -- beginning. If we're going forwards, then apply each origin
       -- from the end.
       --
       -- We have to do a sort any way to ensure ordering, so this is
       -- a convenient way to do it.
       (if places < 0
          then compare
          else flip compare)
       (V.toList origins))

-- | Move an element N places. If the move would go out of the vector,
-- that move is ignored. O(n)
moveElement :: Int -> Int -> Vector a -> Vector a
moveElement origin places vec =
  if places == 0
    then vec
    else if inRange (0, V.length vec - 1) target &&
            inRange (0, V.length vec - 1) origin
           then if places < 0
                  then V.concat
                         [ V.take target vec
                         , V.slice origin 1 vec
                         , V.take (abs places) (V.drop target vec)
                         , V.drop (origin + 1) vec
                         ]
                  else V.concat
                         [ V.take origin vec
                         , V.slice (origin + 1) places vec
                         , V.slice origin 1 vec
                         , V.drop (target + 1) vec
                         ]
           else vec
  where
    target = origin + places

-- | Moving doesn't lose any elements.
prop_preserves_elements :: Int -> Int -> [Int] -> Bool
prop_preserves_elements =
  \i j xs0 ->
    let xs = V.fromList (zip [0 :: Int ..] xs0)
    in List.sort (V.toList (moveElement i j xs)) == List.sort (V.toList xs)
