{-# LANGUAGE TemplateHaskell      #-}

module Types where

import Control.Lens (makeLenses, makePrisms)

data Rank =
    Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Eq, Ord, Enum)

instance Show Rank where
  show Ace = "A"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten ="T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"

data Suit =
    Clubs
  | Diamonds
  | Hearts
  | Spades
  deriving (Eq, Ord, Enum)

instance Show Suit where
  show Clubs = "C"
  show Diamonds = "D"
  show Hearts = "H"
  show Spades = "S"

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Eq, Ord)

instance Show Card where
  show (Card r s) = show r ++ show s

data Query a 
  = Contains [a]
  | Not (Query a)
  | And (Query a) (Query a)
  | Or  (Query a) (Query a)

data Queries a v
  = Q    (Query a) v
  | Qand (Queries a v) (Queries a v)
  | Qor  (Queries a v) (Queries a v)

type QF a = Queries a [a] -> Queries a [a] -> Queries a [a]

data RankPred = RP Rank | WildRank
  deriving Show
makePrisms ''RankPred

data SuitPred = SP Suit | WildSuit
  deriving Show
makePrisms ''SuitPred
  
data CardPred = CardPred
  { _rankPred :: RankPred
  , _suitPred :: SuitPred
  } deriving Show
makeLenses ''CardPred

data BoolAlg a
  = BA a
  | BAnot (BoolAlg a)
  | BAand (BoolAlg a) (BoolAlg a)
  | BAor  (BoolAlg a) (BoolAlg a)

instance (Show a) => Show (BoolAlg a) where
  show (BA a) = show a
  show (BAnot a) = "not " ++ show a
  show (BAand a b) = "(" ++ show a ++ " && " ++ show b ++ ")"
  show (BAor  a b) = "(" ++ show a ++ " || " ++ show b ++ ")"


data Simulation = Simulation
  { _numOfHands :: Int
  , _numOfCards :: Int
  , _trials :: Int
  , _predicates :: [[Card -> Bool]]
  , _queries :: [Query Card]
  , _qOps :: [QF Card]
  } 
makeLenses ''Simulation
