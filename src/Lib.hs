module Lib where

import           Types
import           Utility

import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.Monoid

-- | Standard 52 card deck.
deck :: [Card]
deck = [Card r s | s <- [Clubs .. Spades], r <- [Ace .. King]]

deal :: MonadRandom m => Int -> Int -> [[SimplePredicate]] -> m [[Card]]
deal m n ps = do
  let ps' = take m $ ps <> replicate (m - length ps) []
      go (q:qs) d = do
        h <- fillWithPreds d n q
        t <- go qs (d \\ h)
        return $ h:t
      go [] _ = return []
  go ps' deck
  
simulate :: MonadRandom m => Simulation -> m Double
simulate (Simulation m n ts prds q js) = do
  hands <- replicateM ts $ deal m n prds
  let qs = makeQueries q <$> hands
      bs = foldWithOps1 js <$> qs
      xs = queryDeal <$> bs 
  return $ (fromIntegral $ countTrues xs) / (fromIntegral ts)
  
--  Predicates ------------------------------------------------------------------

isSuit :: Suit -> SimplePredicate
isSuit s c = suit c == s

isRank :: Rank -> SimplePredicate
isRank r c = rank c == r

isPic :: Card -> Bool
isPic c = isRank Jack c || isRank Queen c || isRank King c

nSuit :: Suit -> Int -> [SimplePredicate]
nSuit s n = replicate (min n 13) $ isSuit s

nRank :: Rank -> Int -> [SimplePredicate]
nRank r n = replicate (min n 4) $ isRank r

mkSimplePred :: CardPred -> SimplePredicate
mkSimplePred (CardPred (RP r) (SP s)) c= isRank r c && isSuit s c
mkSimplePred (CardPred (RP r) WildSuit) c = isRank r c
mkSimplePred (CardPred WildRank (SP s)) c = isSuit s c
mkSimplePred (CardPred WildRank WildSuit) _ = True
  
---------------------------------------------------------------------------------

qAnySuit :: Suit -> BoolAlg (Predicate Card)
qAnySuit s = Pure (\c -> suit c == s)

qAnyRank :: Rank -> BoolAlg (Predicate Card)
qAnyRank r = Pure (\c -> rank c == r)

---------------------------------------------------------------------------------
