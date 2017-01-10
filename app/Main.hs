module Main where

import Lib
import Types


main :: IO ()
main = do
  r <- simulate 10000 sim2 (replicate 5 (qAnySuit Spades)) (repeat Qand)
  print r

notSpades = not . isSuit Spades

cp = [nSuit Spades 2 ++ replicate 8 notSpades]

sim2 = Sim 5 10 cp 
