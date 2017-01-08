module Utility where

import Data.List
import Types

countTrues :: [Bool] -> Int
countTrues = sum . map fromEnum

-- Create a vector of length n by padding the initial with elements of source.
fill :: [a] -> [a] -> Int -> ([a], [a])
fill source initial n = (initial ++ rest, source')
  where
    (rest, source') = splitAt (n - length initial) source

fillWithPred :: Eq a => [a] -> [a] -> Int -> (a -> Bool) -> ([a], [a])
fillWithPred source initial n prd = (initial ++ rest, source \\ rest)
  where
    yes  = filter prd source
    rest = take (n - length initial) yes

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
