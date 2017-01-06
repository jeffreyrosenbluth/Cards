module Main where

import Lib
import Types
import Utility

import Control.Monad
import qualified Data.Vector as V

main :: IO ()
main = do
  dks <- replicateM 10000 (shuffle deck52)
  let hs = map (deal sim) dks
  let r = (map . map . fmap) (\(Card _ s) -> if s == Spades then 1 else 0) hs
  let s = (map . map) (\x -> if sum x >= 1 then 1 else 0) r
  let g = map (\x -> if sum x >= 4 then 1 else 0) s
  let h = sum g / 10000
  print h

v2 = V.fromList [Card Two Spades, Card Jack Spades]
v3' = V.fromList [Card Queen Spades, Card Three Diamonds, Card Nine Clubs]

sim = Sim 5 [v2, V.empty, V.empty, V.empty]

test = do
  d <- deck52s
  return $ deal sim d
