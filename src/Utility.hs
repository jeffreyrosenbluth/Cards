module Utility where

import Types

import Control.Monad.Random
import Data.List


randomWithFilter :: MonadRandom m => (a -> Bool) -> [a] -> m a
randomWithFilter _ [] = error "Cannot choose a random element from an empty list"
randomWithFilter prd xs = do
  let xs' = filter prd xs
  r <- getRandomR (0, length xs' - 1)
  return $ xs'!! r

foldWithOps :: [a -> a -> a] -> [a] -> a -> a
foldWithOps (o:os) (x:xs) a = foldWithOps os xs (o x a)
foldWithOps _ _ a = a


foldWithOps1 :: [a -> a -> a] -> [a] -> a
foldWithOps1 os (x:xs) = foldWithOps os xs x
foldWithOps1 _ [] = error "follWithOps1 cannot be applied to an empty list."

countTrues :: [Bool] -> Int
countTrues = sum . map fromEnum

-- Create a vector of length n by padding the initial with elements of source.
fill :: [a] -> [a] -> Int -> ([a], [a])
fill source initial n = (initial ++ rest, source')
  where
    (rest, source') = splitAt (n - length initial) source

-- | Given a source of a\'s, an initial list, and a size n. Create a new
--   list of length n by appending elements of source that satisfy the predicate
--   to initial.
fillWithPred :: Eq a => [a] -> [a] -> Int -> (a -> Bool) -> ([a], [a])
fillWithPred source initial n prd = (initial ++ rest, source \\ rest)
  where
    yes  = filter prd source
    rest = take (n - length initial) yes

fillWithPreds :: (Eq a, MonadRandom m) => [a] -> Int -> [a -> Bool] -> m [a]
fillWithPreds _ 0 _  = return []
fillWithPreds ys m [] = do
  y <- randomWithFilter (const True) ys
  t <- fillWithPreds (delete y ys) (m-1) []
  return $ y:t
fillWithPreds ys m (p:ps) = do
  y <- randomWithFilter p ys
  t <- fillWithPreds (delete y ys) (m-1) ps
  return $ y:t

queryHand :: Eq a => Query a -> [a] -> Bool
queryHand (Contains xs) v = all (flip elem v) xs
queryHand (Not q)       v = not $ queryHand q v
queryHand (And q r)     v = queryHand q v && queryHand r v
queryHand (Or q r)      v = queryHand q v || queryHand r v

queryDeal :: Eq a => Queries a [a] -> Bool
queryDeal (Q q v) = queryHand q v
queryDeal (Qand q r) = queryDeal q && queryDeal r
queryDeal (Qor q r)  = queryDeal q || queryDeal r

makeQueries :: [Query a] -> [[a]] -> [Queries a [a]]
makeQueries = zipWith Q

qAnd :: [Query a] -> Query a
qAnd = foldl1' And

qOr :: [Query a] -> Query a
qOr = foldl1' Or

