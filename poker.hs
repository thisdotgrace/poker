import System.Random
import Data.List

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

-- Shuffling a list
cmp :: (a, Int) -> (a, Int) -> Ordering
cmp (_, y1) (_, y2) = compare y1 y2

--shuffle :: [Card] -> IO [Card]
--shuffle xs = do
--  n <- getRandomInt
--  let shuffled = [ x | (x, a) <- sortBy cmp (zip xs ((randoms (mkStdGen n)) :: [Int]))]
--  return shuffled

shuffle :: RandomGen g => [Card] -> g -> [Card]
shuffle xs gen = 
  let (n, newGen) = randomR (0, maxBound :: Int) gen -- generate random num
      shuffled = [x | (x,_) <- sortBy cmp (zip xs (randoms newGen :: [Int]))]
  in shuffled

-- Player product (can be inactive if folded out)
data Player = 
  Player
  { name :: String,
    privateCards :: Maybe [Card],
    chips :: Int,
    isDealer :: Bool,
    isActive :: Bool  -- to track if player is still in round (ie not folded)
    -- Behaviour :: String?
  }
  deriving (Show)


data GameState = 
  GameState
  { players :: [Player],
    currentDeck :: [Card],
    stage :: String,
    communityCards :: Maybe [Card],     -- nothing intially, just cards when dealt
    pot :: Maybe Int,                   
    bets :: Maybe [(Player, Int)],
    dealer :: Maybe String,
    smallBlind :: Maybe String,
    bigBlind :: Maybe String
  }
  deriving (Show)

-- Sample GameState with instantiated data
exampleGameState :: GameState
exampleGameState = GameState
  { players = [Player "Alice" (Just [Card Two Hearts, Card Three Spades]) 1000 True True,
               Player "Bob" (Just [Card Four Diamonds, Card Ace Clubs]) 1500 False True],
    currentDeck = deck,
    stage = "Pre-Flop", -- or another stage like "Flop", "Turn", "River"
    communityCards = Nothing,  -- No community cards yet
    pot = Just 500,           -- Example pot amount
    bets = Just [(Player "Alice" (Just [Card Two Hearts, Card Three Spades]) 1000 True True, 100),
                 (Player "Bob" (Just [Card Four Diamonds, Card Ace Clubs]) 1500 False True, 200)],
    dealer = Just "Alice",    -- Alice is the dealer
    smallBlind = Just "Bob",  -- Bob is the small blind
    bigBlind = Just "Alice"   -- Alice is the big blind
  }

  -- initial game state (setup)
  -- Initialize players separately
player1 = Player { name = "Alice", privateCards = Nothing, chips = 100, isDealer = False, isActive = True }
player2 = Player { name = "Bob", privateCards = Nothing, chips = 100, isDealer = False, isActive = True }
player3 = Player { name = "Charlie", privateCards = Nothing, chips = 100, isDealer = False, isActive = True }
player4 = Player { name = "David", privateCards = Nothing, chips = 100, isDealer = False, isActive = True }

-- Define a list of players
playersList :: [Player]
playersList = [player1, player2, player3, player4]

-- Function to initialize the GameState
initializeGameState :: [Player] -> [Card] -> GameState
initializeGameState players deck = GameState
  { players = playersList,
    currentDeck = shuffledDeck,
    stage = "Pre-Flop",  -- Start with Pre-Flop stage
    communityCards = Nothing,  -- No community cards initially
    pot = Nothing,  -- Pot starts as Nothing
    bets = Nothing,  -- Bets start as Nothing
    dealer = Nothing,  -- First player is the dealer
    smallBlind = Nothing,  -- Second player is the small blind
    bigBlind = Nothing  -- Third player is the big blind
  }
  where
    -- Shuffle the deck
    shuffledDeck = shuffle deck (mkStdGen 42)



dealPrivateCards :: Player -> [Card] -> Player
dealPrivateCards player cards = player {privateCards = Just (take 2 cards)}

dealCardsToAll :: [Player] -> [Card] -> ([Player], [Card])
dealCardsToAll [] deck = ([], deck)
dealCardsToAll (x:xs) deck =
  let updatedPlayer = dealPrivateCards x deck                         -- deal cards to current player (update their state)
      remainingDeck = drop 2 deck                                     -- remove 2 cards from deck
      (updatedPlayers, finalDeck) = dealCardsToAll xs remainingDeck   -- recursively call, all players
  in (updatedPlayer : updatedPlayers, finalDeck)                      -- return updated players and remaining deck of cards

-- dealCards :: GameState -> [Player] -> GameState

-- deal cards
-- with shuffled deck, take 2 cards for each player
-- dealCards state players = 

-- for each player, create a new player with 2 cards from deck, update deck to reflect cards taken off top
-- dealCards

dealCards :: GameState -> String
dealCards gameState
  | stage gameState =="Pre-Flop" = "Need to deal private cards"
  | otherwise = "Flop?"


testDeck :: IO()
testDeck = do
    putStrLn "Original deck:"
    print deck
    putStrLn "\nShuffled deck:"
    let gen = mkStdGen 42
    let shuffledDeck = shuffle deck gen
    print shuffledDeck

    let player1 = Player{name = "Grace", privateCards = Nothing, chips = 100, isDealer = True, isActive = True}
    print player1

    let player1WithCards = dealPrivateCards player1 (take 2 shuffledDeck)
    print player1WithCards


testGameState :: GameState -> IO()
testGameState gameState = do
  putStrLn "Game State"
  print gameState

testDealing :: IO()
testDealing = do
  let gen = mkStdGen 42
  let shuffledDeck = shuffle deck gen
  let (updatedPlayers, remainingDeck) = dealCardsToAll playersList shuffledDeck
  putStrLn "Number of Players:"
  print (length updatedPlayers)
  putStrLn "Updated Players:"
  print updatedPlayers
  putStrLn "Remaining Deck:"
  print remainingDeck
  putStrLn "Cards remaining: "
  print (length remainingDeck)



testSetupState :: IO()
testSetupState = do
  let gameState = initializeGameState playersList deck
  print gameState


displayGameState :: GameState -> IO()
displayGameState gameState = do
  putStrLn "========================"
  putStrLn "Game State:"
  putStrLn "========================"

  -- print Players
  putStrLn "Players:"
  mapM_ (putStrLn . showPlayer) (players gameState)
    
  -- print Current Deck
  putStrLn "\nCurrent Deck:"
  showDeck (currentDeck gameState)
    
  -- print Stage
  putStrLn "\nStage:"
  putStrLn $ stage gameState
    
  -- print Community Cards
  putStrLn "\nCommunity Cards:"
  case communityCards gameState of
    Nothing -> putStrLn "None"
    Just cards -> showDeck cards
    
  -- print Pot
  putStrLn "\nPot:"
  case pot gameState of
    Nothing -> putStrLn "None"
    Just amount -> putStrLn $ "Total Pot: " ++ show amount
    
  -- print Bets
  putStrLn "\nBets:"
  case bets gameState of
    Nothing -> putStrLn "No bets placed yet."
    Just betList -> mapM_ showBet betList
    
  -- print Dealer Info
  putStrLn "\nDealer:"
  case dealer gameState of
    Nothing -> putStrLn "No dealer."
    Just d -> putStrLn $ "Current dealer: " ++ d
    
  -- print Small and Big Blinds
  putStrLn "\nBlinds:"
  putStrLn $ "Small Blind: " ++ (maybe "None" id (smallBlind gameState))
  putStrLn $ "Big Blind: " ++ (maybe "None" id (bigBlind gameState))
    
  putStrLn "==============================="
  putStrLn ""

-- helper function to display a Player
showPlayer :: Player -> String
showPlayer p = name p ++ " (Chips: " ++ show (chips p) ++ ") \n (Cards: " ++ show (privateCards p) ++ ")"

-- helper function to print the deck of cards
showDeck :: [Card] -> IO ()
showDeck [] = putStrLn "No cards."
showDeck cards = putStrLn $ unwords (map show cards)

-- helper function to print a bet (Player and amount)
showBet :: (Player, Int) -> IO ()
showBet (player, betAmount) = putStrLn $ name player ++ " bet: " ++ show betAmount

setupState :: IO()
setupState = do
  let gameState = initializeGameState playersList deck
  displayGameState gameState

  let cards = currentDeck gameState
  let currentPlayers = players gameState
  putStrLn "Players:"
  mapM_ (putStrLn . showPlayer) (currentPlayers)
  let (newPlayers, newCards) = dealCardsToAll currentPlayers cards

  let newGameState = gameState { players = newPlayers, currentDeck = newCards }

  displayGameState newGameState


main :: IO()
main = do
  setupState

