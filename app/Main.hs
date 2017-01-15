{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Types
import Compile
import DSL

import Data.Text.Lazy hiding (replicate, repeat, filter, length)
import Prelude        hiding (concat)


main :: IO ()
main = do
  let (Right p) = parseStatement whist0
  c <- compile p initialSimulation
  putStrLn $ "Hands: " ++ show (numOfHands c)
  putStrLn $ "Cards: " ++ show (numOfCards c)
  putStrLn $ "Trials: " ++ show (trials c)
  let d = c {queries = replicate 5 qSpade, qOps = repeat Qand}
  e <- simulate d
  print $ result e
  
  -- s <- simulate simu
  -- print (result s)

notSpades :: CardPredicate
notSpades = not . isSuit Spades

cp :: [[CardPredicate]]
cp = [nSuit Spades 2 ++ replicate 8 notSpades]

simu :: Simulation
simu = Simulation 5 10 10000 cp
                 (replicate 5 qSpade)
                 (repeat Qand)
                 [0]

qSpade :: HandPredicate
qSpade [] = False
qSpade xs = (length $ filter (isSuit Spades) xs) > 0

  
whist0 :: Text
whist0 = concat [l1,l2,l3,l4]
  where
    l1 = "Hands 5;"
    l2 = "Cards 10;"
    l3 = "Trials 10000;"
    l4 = "Predicate 0 [AS, *S, ~*S, ~*S, ~*S, ~*S, ~*S, ~*S, ~*S, ~*S];"
    -- l5 = "Query 0 *S;"
    -- l6 = "Query 1 *S;"
    -- l7 = "Query 2 *S;"
    -- l8 = "Query 3 *S;"
    -- l9 = "Query 4 *S;"
