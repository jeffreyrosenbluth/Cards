module DSL where

import           Types

import           Control.Applicative       (empty)
import           Control.Monad             (void)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer     as L
import           Text.Megaparsec.Text.Lazy

-- Lexer --------------------------------------------------------------------------------
  
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt empty
  where lineCmnt  = L.skipLineComment "--"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

semi :: Parser String
semi = symbol ";"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

int :: Parser Int
int = fromIntegral <$> integer

-- Parser -------------------------------------------------------------------------------
-- Statements
  
statement :: Parser Statement
statement = parens statement <|> statementSeq

statementSeq :: Parser Statement
statementSeq = f <$> sepBy1 statement' semi
  -- if there's only one stmt return it without using ‘Seq’
  where f l = if length l == 1 then head l else Statements l

statement' :: Parser Statement
statement' = setNumOfHands
         <|> setNumOfCards
         <|> setNumOfTrials
         <|> setPredicate
         <|> run

setNumOfHands :: Parser Statement
setNumOfHands = SetNumOfHands <$> (symbol "Hands" *> int)

setNumOfCards :: Parser Statement
setNumOfCards = SetNumOfCards <$> (symbol "Cards" *> int)

setNumOfTrials :: Parser Statement
setNumOfTrials = SetNumOfTrials <$> (symbol "Trials" *> int)

setPredicate :: Parser Statement
setPredicate = SetPredicate <$> (symbol "Predicate" *> int) <*> baList

run :: Parser Statement
run = Run <$ symbol "Run"

-- Predicates

rankParser :: Parser Rank
rankParser =
  toRank <$> oneOf ['A', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K']
  where
    toRank 'A' = Ace
    toRank '2' = Two
    toRank '3' = Three
    toRank '4' = Four
    toRank '5' = Five
    toRank '6' = Six
    toRank '7' = Seven
    toRank '8' = Eight
    toRank '9' = Nine
    toRank 'T' = Ten
    toRank 'J' = Jack
    toRank 'Q' = Queen
    toRank 'K' = King
    toRank _   = error "Not a valid rank"

rankPredParser :: Parser RankPred
rankPredParser = RP <$> rankParser <|> WildRank <$ symbol "*"

suitParser :: Parser Suit
suitParser = toSuit <$> oneOf ['C', 'D', 'H', 'S']
  where
    toSuit 'C' = Clubs
    toSuit 'D' = Diamonds
    toSuit 'H' = Hearts
    toSuit 'S' = Spades
    toSuit _   = error "Not a valid Suit"
    
suitPredParser :: Parser SuitPred
suitPredParser =  SP <$> suitParser <|> WildSuit <$ symbol "*"

cardPredParser :: Parser CardPred
cardPredParser = CardPred <$> rankPredParser <*> lexeme suitPredParser

baCardPred :: Parser (CardPredAlg)
baCardPred = makeExprParser term table

term :: Parser (CardPredAlg)
term = parens baCardPred <|> Pure <$> cardPredParser 

table :: [[Operator Parser (CardPredAlg)]]
table = [ [ prefix  "~"  Not ]
        , [ binary  "&&" And
          , binary  "||" Or
          ]
        ]

binary :: String -> (CardPredAlg -> CardPredAlg -> CardPredAlg)
       -> Operator Parser (CardPredAlg)
binary name f = InfixL (f <$ symbol name)

prefix :: String -> (CardPredAlg -> CardPredAlg)
       -> Operator Parser (CardPredAlg)
prefix name f = Prefix (f <$ symbol name)

baList :: Parser [CardPredAlg]
baList = symbol "[" *> sepBy1 baCardPred comma <* symbol "]"
