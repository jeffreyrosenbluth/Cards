module Compile where

import Types
import DSL
import Lib
import Utility

import Control.Monad.Random

modify :: [a] -> Int -> a -> [a]
modify [] _ _ = []
modify xs n y
  | null ts = xs
  | otherwise = ss ++ (y:ys)
  where
    (ss, ts@(_:ys)) = splitAt n xs


initialSimulation :: Simulation
initialSimulation =
  Simulation 0 0 0 [[]] [] [] []
  
compile :: MonadRandom m => Statement -> Simulation -> m Simulation
compile (SetNumOfHands n) s = return $ s {numOfHands = n}
compile (SetNumOfCards n) s = return $ s {numOfCards = n}
compile (SetNumOfTrials n) s = return $ s {trials =  n}
compile (SetPredicate n cps) s = return $ s {predicates = psn}
  where
    psn = modify (predicates s) n ps
    ps  = mkPredicate <$> (fmap . fmap) mkCardPred  cps
compile (SetQuery n cp) s = return $ s {queries = qn}
  where
    qn = modify (queries s) n q
    q = mkPredicate $ mkCardPred <$> cp
compile Run s = simulate s
compile (Statements []) s = return s
compile (Statements (x:xs)) s = compile x s >>= compile (Statements xs)


  
