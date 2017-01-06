module Utility where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Random
import qualified Data.Vector.Generic          as V
import           Data.Vector.Generic         (Vector)
import qualified Data.Vector.Generic.Mutable as MV


shuffle :: (PrimMonad m, MonadRandom m, Vector v a) => v a -> m (v a)
shuffle v = do
  arr' <- V.unsafeThaw v
  arr <- MV.clone arr'
  forM_ [0.. MV.length arr - 2] $ \i -> do
    j <- getRandomR (i, MV.length arr - 1)
    MV.unsafeSwap arr i j
  V.unsafeFreeze arr

foldRange :: (Vector v b) => (a -> b -> a) -> a -> Int -> Int -> v b -> a
foldRange f a start end vec = go start a
  where
    go idx accum
      | idx < end + 1 = go (idx + 1) (f accum (vec V.! idx))
      | otherwise = accum

foldRanges :: (Vector v b) => (a -> b -> a) -> a -> [Int] -> v b -> [a]
foldRanges f a rs vec = fmap fr rs'
  where
    fr (i, j) = foldRange f a i (j-1) vec
    rs'  = zip (0 : rs) rs 

-- Create a vector of length n by padding the initial with elements of source.
fill :: (Vector v a) => v a -> v a -> Int -> (v a, v a)
fill source initial n = (initial V.++ rest, source')
  where
    (rest, source') = V.splitAt (n - V.length initial) source
