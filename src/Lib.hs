module Lib where

import Types
import Utility

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Monoid

-- | Standard 52 card deck.
deck :: [Card]
deck = [Card r s | s <- [Clubs .. Spades], r <- [Ace .. King]]

deal :: MonadRandom m => Int -> Int -> [[Card -> Bool]] -> m [[Card]]
deal m n ps = do
  let ps' = take m $ ps <> replicate (m - length ps) []
      go (q:qs) d = do
        h <- fillWithPreds d n q
        t <- go qs (d \\ h)
        return $ h:t
      go [] _ = return []
  go ps' deck
  
simulate :: MonadRandom m
          => Int -> Sim -> [Query Card] -> [QF Card]
          -> m Double
simulate trials (Sim m n prds) q js  = do
  hands <- replicateM trials $ deal m n prds
  let qs = makeQueries q <$> hands
      bs = foldWithOps1 js <$> qs
      xs = queryDeal <$> bs 
  return $ (fromIntegral $ countTrues xs) / (fromIntegral trials)
  
--  Predicates ------------------------------------------------------------------
isSuit :: Suit -> Card -> Bool
isSuit s c = suit c == s

isRank :: Rank -> Card -> Bool
isRank r c = rank c == r

isPic :: Card -> Bool
isPic c = isRank Jack c || isRank Queen c || isRank King c

nSuit :: Suit -> Int -> [Card -> Bool]
nSuit s n = replicate (min n 13) $ isSuit s

nRank :: Rank -> Int -> [Card -> Bool]
nRank r n = replicate (min n 4) $ isRank r
-------------------------------------------------------------------------------
qAnySuit :: Suit -> Query Card
qAnySuit s = qOr [Contains [Card r s] | r <- [Ace .. King]]

qAnyRank :: Rank -> Query Card
qAnyRank r = qOr [Contains [Card r s] | s <- [Clubs .. Spades]]
-------------------------------------------------------------------------------
