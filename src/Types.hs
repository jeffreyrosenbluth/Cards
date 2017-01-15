{-# LANGUAGE DeriveFunctor #-}

module Types where

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
  
data BoolAlg a
  = Pure a
  | Not (BoolAlg a)
  | And (BoolAlg a) (BoolAlg a)
  | Or  (BoolAlg a) (BoolAlg a)
  deriving Functor

instance (Show a) => Show (BoolAlg a) where
  show (Pure a) = show a
  show (Not a) = "not " ++ show a
  show (And a b) = "(" ++ show a ++ " && " ++ show b ++ ")"
  show (Or  a b) = "(" ++ show a ++ " || " ++ show b ++ ")"

type Predicate a = a -> Bool
  
data Query a v
  = Q    (Predicate a) v
  | Qand (Query a v) (Query a v)
  | Qor  (Query a v) (Query a v)

type QF a = Query [a] [a] -> Query [a] [a] -> Query [a] [a]

data RankPattern = RP Rank | WildRank
  deriving Show

data SuitPattern = SP Suit | WildSuit
  deriving Show
  
data CardPattern = CardPattern
  { rankPattern :: RankPattern
  , suitPattern :: SuitPattern
  } deriving Show

type CardPredicate = Predicate Card
type HandPredicate = Predicate [Card]

data Simulation = Simulation
  { numOfHands :: Int
  , numOfCards :: Int
  , trials :: Int
  , predicates :: [[CardPredicate]]
  , queries :: [HandPredicate]
  , qOps :: [QF Card]
  , result :: [Double]
  } 

type CardPatternAlg = BoolAlg CardPattern

data Statement
  = SetNumOfHands Int
  | SetNumOfCards Int
  | SetNumOfTrials Int
  | SetPredicate Int [CardPatternAlg]
  | SetQuery Int CardPatternAlg
  | Run
  | Statements [Statement]
  deriving Show
