{-# LANGUAGE StrictData #-}

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

data Sim = Sim
  { numHands :: Int
  , cardsPerHand :: Int
  , knownCards :: [[Card]]
  , predicates :: [Card -> Bool]
  } 

data Query a 
  = Contains [a]
  | Not (Query a)
  | And (Query a) (Query a)
  | Or  (Query a) (Query a)

data Queries a v
  = Q    (Query a) v
  | Qand (Queries a v) (Queries a v)
  | Qor  (Queries a v) (Queries a v)

data Junction = Some | All
  deriving Eq
