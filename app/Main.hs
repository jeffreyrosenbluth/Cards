module Main where

import Lib
import Types
import Utility

import Control.Monad
import System.Random.Shuffle

main :: IO ()
main = do
  r <- simulate 10000 sim2 (replicate 5 (qAnySuit Spades)) (repeat Qand)  deck
  print r

v10  = [ Card Ace Spades
      , Card Two Spades
      , Card Three Clubs
      , Card Four Clubs
      , Card Five Clubs
      , Card Six Clubs
      , Card Seven Clubs
      , Card Eight Clubs
      , Card Nine Clubs
      , Card Ten Clubs
      ]

v3 = [Card Queen Spades, Card Three Diamonds, Card Nine Clubs]

v2 = [Card Ace Spades, Card Two Spades]

p2 = not . isSuit Spades

sim10 = Sim 5 10 [v10] []

sim2 = Sim 5 10 [v2] [p2]

testDeal :: IO [[Card]]
testDeal = deal sim2 deck
