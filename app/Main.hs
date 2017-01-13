module Main where

import Lib
import Types


main :: IO ()
main = do
  r <- simulate simu
  print r

notSpades :: Card -> Bool
notSpades = not . isSuit Spades

cp :: [[Card -> Bool]]
cp = [nSuit Spades 2 ++ replicate 8 notSpades]

simu :: Simulation
simu = Simulation 5 10 10000 cp (replicate 5 (qAnySuit Spades)) (repeat Qand)
