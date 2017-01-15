module Utility where

import          Types

import          Control.Monad.Random
import          Data.List

randomChoice :: MonadRandom m => [a] -> m a
randomChoice [] = error "Cannot choose a radnom element from an empty list"
randomChoice xs = do
  r <- getRandomR (0, length xs -1)
  return $ xs !! r

randomWithFilter :: MonadRandom m => (Predicate a) -> [a] -> m a
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

-- 17.8 27
fillWithPreds :: (Eq a, MonadRandom m) => [a] -> Int -> [Predicate a] -> m [a]
fillWithPreds _ 0 _  = return []
fillWithPreds ys m [] = do
  y <- randomChoice ys
  t <- fillWithPreds (delete y ys) (m-1) []
  return $ y:t
fillWithPreds ys m (p:ps) = do
  y <- randomWithFilter p ys
  t <- fillWithPreds (delete y ys) (m-1) ps
  return $ y:t

mkPredicate :: BoolAlg (Predicate a) -> Predicate a
mkPredicate (Pure p) = p
mkPredicate (Not p) = not . mkPredicate p
mkPredicate (And p q) = \a -> mkPredicate p a && mkPredicate q a
mkPredicate (Or  p q) = \a -> mkPredicate p a || mkPredicate q a

queryDeal :: Eq a => Query [a] [a] -> Bool
queryDeal (Q q v) = q v
queryDeal (Qand q r) = queryDeal q && queryDeal r
queryDeal (Qor q r)  = queryDeal q || queryDeal r

makeQueries :: [Predicate a] -> [v] -> [Query a v]
makeQueries = zipWith Q 

qAnd :: [BoolAlg a] -> BoolAlg a
qAnd = foldl1' And

qOr :: [BoolAlg a] -> BoolAlg a
qOr = foldl1' Or

