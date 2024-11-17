

-- Defines sum types for suit and value 
data Suit = Hearts | Diamonds | Spades | Clubs 
            deriving (Eq, Show)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
             | Jack | Queen | King | Ace 
             deriving (Eq, Show, Enum, Bounded) -- enum allows to create range (ie [Two..Ace]), bounded gives minBound/maxBound
data Card = Card Value Suit 
            deriving (Eq)
instance Show Card where -- show instance so cards displayed in form 'value of suit'
  show (Card value suit) = show value ++ " of " ++ show suit


-- Creates deck of cards using list comprehension
deck :: [Card]
deck = [ Card value suit | suit <- [Hearts, Diamonds, Spades, Clubs], value <- [Two .. Ace] ]

-- Player product (can be inactive if folded out)
data Player = 
  Inactive |
  Active
  { name :: String,
    privateCards :: [Card],
    chips :: Int,
    isDealer :: Bool
    -- Behaviour :: String?
  }
  deriving (Show)




main :: IO()
main = print deck
