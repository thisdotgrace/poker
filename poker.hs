
data Suit = Hearts | Diamonds | Spades | Clubs 
            deriving (Eq, Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
             | Jack | Queen | King | Ace 
             deriving (Eq, Show, Enum, Bounded)
             -- enum allows to create range (ie [Two..Ace]), and values can be ordered
             -- bounded gives minBound, maxBound

data Card = Card Value Suit 
            deriving (Eq)

-- show instance so cards displayed in form 'value of suit'
instance Show Card where
  show (Card value suit) = show value ++ " of " ++ show suit


-- Creates deck of cards using list comprehension
deck :: [Card]
deck = [ Card value suit | suit <- [Hearts, Diamonds, Spades, Clubs], value <- [Two .. Ace] ]


main :: IO()
main = print deck
