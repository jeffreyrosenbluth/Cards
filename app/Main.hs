module Main where

import Lib
import Types
import Utility


main :: IO ()
main = do
  r <- simulate simu
  print r

notSpades :: SimplePredicate
notSpades = not . isSuit Spades

cp :: [[SimplePredicate]]
cp = [nSuit Spades 2 ++ replicate 8 notSpades]

simu :: Simulation
simu = Simulation 5 10 10000 cp
                 (replicate 5 (mkPredicate $ qAnySuit Spades))
                 (repeat Qand)
