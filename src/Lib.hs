module Lib where

import Types
import Utility

import Control.Monad.Random
import Data.List
import Data.Monoid
import System.Random.Shuffle

deck :: [Card]
deck = [Card r s | s <- [Clubs .. Spades], r <- [Ace .. King]]

deckS :: (MonadRandom m) => m [Card]
deckS = shuffleM deck

-- | Deal the cards to all of the hands in the game such that each hand
--   gets is known cards and the remainder satisfy the predicate. The deck
--   does not have to be shuffled.
deal :: MonadRandom m => Sim -> [Card] -> m [[Card]]
deal (Sim m n cs ps) d = do
  -- Make sure to shuffle after removing all of the fixed cards in each
  -- hand.
  dk <- shuffleM (d \\ mconcat cs)
  -- Use the number of hands to guarantee that the lists of know cards
  -- and predicates are both of the right length.
  let kcs = take m $ cs <> repeat []
      prds = take m $ ps <> repeat (const True)
      go (x:xs) (q:qs) y = do
        let (h, d') = fillWithPred y x n q
        -- Shuffle the remaining cards after they are filtered, otherwise
        -- the reamining cards will be biased so that the front of the deck
        -- does not satisfy the predicate.
        d'' <- shuffleM d'
        t   <- go xs qs d''
        return $ h:t
      go [] [] _ = return []
      go _ _ _ = error "The sky is falling."
  go kcs prds dk

type QF = Queries Card [Card] -> Queries Card [Card] -> Queries Card [Card]
-- | Simulate and experiment with initializtion record Sim and a list
--   of queries.
simulate :: MonadRandom m => Int -> Sim -> [Query Card] -> [QF] -> [Card] -> m Double
simulate trials sim q js d = do
  let dks = replicate trials d
  hands <- traverse (deal sim) dks
  let qs = makeQueries q <$> hands
      bs = foldWithOps1 js <$> qs
      -- bs = foldl1' (if j == Some then Qor else Qand) <$> qs
      xs = queryDeal <$> bs 
  return $ (fromIntegral $ countTrues xs) / (fromIntegral trials)
  
--  Predicates ------------------------------------------------------------------
isSuit :: Suit -> Card -> Bool
isSuit s c = suit c == s

isRank :: Rank -> Card -> Bool
isRank r c = rank c == r

isPic :: Card -> Bool
isPic c = isRank Jack c || isRank Queen c || isRank King c
-------------------------------------------------------------------------------

qAnySuit :: Suit -> Query Card
qAnySuit s = qOr [Contains [Card r s] | r <- [Ace .. King]]

qAnyRank :: Rank -> Query Card
qAnyRank r = qOr [Contains [Card r s] | s <- [Clubs .. Spades]]
-------------------------------------------------------------------------------
