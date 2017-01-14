{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Types
import Utility

import Data.Text.Lazy hiding (replicate, repeat)
import Prelude        hiding (concat)


main :: IO ()
main = do
  s <- simulate simu
  print (result s)

notSpades :: CardPredicate
notSpades = not . isSuit Spades

cp :: [[CardPredicate]]
cp = [nSuit Spades 2 ++ replicate 8 notSpades]

simu :: Simulation
simu = Simulation 5 10 10000 cp
                 (replicate 5 (mkPredicate $ qAnySuit Spades))
                 (repeat Qand)
                 [0]

whist0 :: Text
whist0 = concat [l1,l2,l3,l4,l5,l6,l7,l8,l9]
  where
    l1 = "Hands 5;"
    l2 = "Cards 10;"
    l3 = "Trials 10000;"
    l4 = "Predicate 0 [AS, *S, ~*S, ~*S, ~*S, ~*S, ~*S, ~*S, ~*S, ~*S];"
    l5 = "Query 0 *S;"
    l6 = "Query 1 *S;"
    l7 = "Query 2 *S;"
    l8 = "Query 3 *S;"
    l9 = "Query 4 *S"
