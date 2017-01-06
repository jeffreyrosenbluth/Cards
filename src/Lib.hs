module Lib where

import Types
import Utility

import Data.Vector as V

someFunc :: IO ()
someFunc = putStrLn "someFunc"

deck52 :: Deck
deck52 = V.fromList [Card r s | r <- [Ace .. King], s <- [Clubs .. Spades]]

deck52s :: IO (Vector Card)
deck52s = shuffle deck52

suits :: Suit -> Deck -> [Int]
suits s = foldRanges f 0 [13,26,39,52] 
  where
    f a (Card _ t)
      | s == t = a + 1
      | otherwise = a

deal :: Sim -> Deck-> [V.Vector Card]
deal (Sim _ []) _      = []
deal (Sim n (c:cs)) d  = h : deal (Sim n cs) d'
  where
    (h, d') = fill d c n
