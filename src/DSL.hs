module DSL where

import           Types

import           Control.Monad             (void)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer     as L
import           Text.Megaparsec.Text.Lazy


sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

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

baCardPred :: Parser (BoolAlg CardPred)
baCardPred = makeExprParser term table

term :: Parser (BoolAlg CardPred)
term = parens baCardPred <|> BA <$> cardPredParser 

table :: [[Operator Parser (BoolAlg CardPred)]]
table = [ [ prefix  "~"  BAnot ]
        , [ binary  "&&" BAand
          , binary  "||" BAor
          ]
        ]

binary :: String -> (BoolAlg CardPred -> BoolAlg CardPred -> BoolAlg CardPred)
       -> Operator Parser (BoolAlg CardPred)
binary  name f = InfixL  (f <$ symbol name)

prefix :: String -> (BoolAlg CardPred -> BoolAlg CardPred)
       -> Operator Parser (BoolAlg CardPred)
prefix  name f = Prefix  (f <$ symbol name)

baList :: Parser [BoolAlg CardPred]
baList = symbol "[" *> sepBy1 baCardPred comma <* symbol "]"
