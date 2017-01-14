module Main where

import Lib
import Types
import Utility


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
