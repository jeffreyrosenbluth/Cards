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

deal :: MonadRandom m => Int -> Int -> [[CardPredicate]] -> m [[Card]]
deal m n ps = do
  let ps' = take m $ ps <> replicate (m - length ps) []
      go (q:qs) d = do
        h <- fillWithPreds d n q
        t <- go qs (d \\ h)
        return $ h:t
      go [] _ = return []
  go ps' deck
  
simulate :: MonadRandom m => Simulation -> m Simulation
simulate s@(Simulation m n ts prds q rs) = do
  hands <- replicateM ts $ deal m n prds
  let qs = q <$> hands
      r  = (fromIntegral $ countTrues qs) / (fromIntegral ts)
  return $ s {result = rs ++ [r]}
  
--  Predicates ------------------------------------------------------------------

isSuit :: Suit -> CardPredicate
isSuit s c = suit c == s

isRank :: Rank -> CardPredicate
isRank r c = rank c == r

isPic :: CardPredicate
isPic c = isRank Jack c || isRank Queen c || isRank King c

nSuit :: Suit -> Int -> [CardPredicate]
nSuit s n = replicate (min n 13) $ isSuit s

nRank :: Rank -> Int -> [CardPredicate]
nRank r n = replicate (min n 4) $ isRank r

mkCardPred :: CardPattern -> CardPredicate
mkCardPred (CardPattern (RP r) (SP s)) c     = isRank r c && isSuit s c
mkCardPred (CardPattern (RP r) WildSuit) c   = isRank r c
mkCardPred (CardPattern WildRank (SP s)) c   = isSuit s c
mkCardPred (CardPattern WildRank WildSuit) _ = True

atLeastOne :: Suit -> HandsPredicate
atLeastOne s = all (any $ isSuit s)

hasHighest :: Suit -> HandsPredicate
hasHighest _ [] = False
hasHighest s (h:hs) = case filter (isSuit s)  h of
  [] -> False
  xs -> (maximum $ map rank xs) > m
    where
      m = case filter (isSuit s) (concat hs) of
        [] -> Two
        ms -> maximum $ map rank ms

  
---------------------------------------------------------------------------------

qAnySuit :: Suit -> BoolAlg (Predicate Card)
qAnySuit s = Pure (\c -> suit c == s)

qAnyRank :: Rank -> BoolAlg (Predicate Card)
qAnyRank r = Pure (\c -> rank c == r)

---------------------------------------------------------------------------------
