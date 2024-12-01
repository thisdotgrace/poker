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
shuffle :: RandomGen g => [Card] -> g -> [Card]
shuffle xs gen = 
  let (n, newGen) = randomR (0, maxBound :: Int) gen -- generate random num
      shuffled = [x | (x,_) <- sortBy cmp (zip xs (randoms newGen :: [Int]))]
  in shuffled

data PlayerRole = Dealer | SmallBlind | BigBlind | Regular
  deriving (Eq, Show)

-- Player product type
data Player = 
  Player
  { name :: String,
    privateCards :: Maybe [Card],
    chips :: Int,
    role :: PlayerRole,
    isActive :: Bool  -- to track if player is still in round (ie not folded)
    -- Behaviour :: String?
  }
  deriving (Show)

-- GameState product type
data GameState = 
  GameState
  { players :: [Player],
    currentDeck :: [Card],
    stage :: String,
    communityCards :: Maybe [Card],     -- nothing intially, just cards when dealt
    pot :: Maybe Int,                   
    bets :: Maybe [(Player, Int)]
  }
  deriving (Show)




setupPlayers :: IO [Player]
setupPlayers = do
  -- ask for number of players
  putStrLn "Enter number of players: "
  numPlayersStr <- getLine
  let numPlayers = read numPlayersStr :: Int

  -- get player names
  putStrLn $ "Enter the names of " ++ show numPlayers ++ " players:"

  -- collect names and create players
  getPlayerNames numPlayers 1 []
  

-- helper function to recursively get player names
getPlayerNames :: Int -> Int -> [Player] -> IO [Player]
getPlayerNames 0 _ players = return players -- base case, when no more players to add
getPlayerNames remaining count players = do
  -- ask for next players name
  putStrLn $ "Enter the name for player " ++ show count ++ ":"
  playerName <- getLine

  -- create new player and add to list of players
  let newPlayer = Player {name = playerName, privateCards = Nothing, chips = 100, role = Regular, isActive = True}
  getPlayerNames (remaining - 1) (count + 1) (players ++ [newPlayer])


-- Takes 2 cards off top of the deck and assigns them to a player
dealPrivateCards :: Player -> [Card] -> Player
dealPrivateCards player cards = player {privateCards = Just (take 2 cards)}

-- applies this to all players, and returns an updated player list and updated cards (with dealt cards removed)
dealCardsToAll :: [Player] -> [Card] -> ([Player], [Card])
dealCardsToAll [] deck = ([], deck)
dealCardsToAll (x:xs) deck =
  let updatedPlayer = dealPrivateCards x deck                         -- deal cards to current player (update their state)
      remainingDeck = drop 2 deck                                     -- remove 2 cards from deck
      (updatedPlayers, finalDeck) = dealCardsToAll xs remainingDeck   -- recursively call, all players
  in (updatedPlayer : updatedPlayers, finalDeck)                      -- return updated players and remaining deck of cards

-- identify dealer
-- assign smallblind/bigblind as next two players in sequence after dealer
-- update the players roles in the game state
assignDealerAndBlinds :: [Player] -> [Player]
assignDealerAndBlinds players = 
  let
    numPlayers = length players
    currentDealerIndex = 
      case findIndex (\ player -> role player == Dealer) players of
        Just idx -> idx
        Nothing -> -1
    -- finds index of current dealer, default to 0 for 1st round

    -- calculate indices for new dealer/smallblind/bigblind
    nextDealerIndex = (currentDealerIndex + 1) `mod` numPlayers
    smallBlindIndex = (nextDealerIndex + 1) `mod` numPlayers
    bigBlindIndex = (nextDealerIndex + 2) `mod` numPlayers

    -- update player roles
    updateRole :: Int -> Player -> Player
    updateRole i player
      | i == nextDealerIndex = player {role = Dealer }
      | i == smallBlindIndex = player {role = SmallBlind }
      | i == bigBlindIndex   = player {role = BigBlind }
      | otherwise            = player {role = Regular}
  in
    zipWith updateRole [0..] players

-- when placing a bet, check if player has enough
-- check if bet satisfies (double, match etc)
-- placeBet :: Player -> Int -> Int -> GameState

-- Displays a given game state
displayGameState :: GameState -> IO()
displayGameState gameState = do
  putStrLn "========================"
  putStrLn "Game State:"
  putStrLn "========================"

  -- print players
  putStrLn "Players:"
  mapM_ (putStrLn . showPlayer) (players gameState)
    
  -- print current Deck
  putStrLn "\nCurrent Deck:"
  showDeck (currentDeck gameState)
    
  -- print stage
  putStrLn "\nStage:"
  putStrLn $ stage gameState
    
  -- print community Cards
  putStrLn "\nCommunity Cards:"
  case communityCards gameState of
    Nothing -> putStrLn "None"
    Just cards -> showDeck cards
    
  -- print pot
  putStrLn "\nPot:"
  case pot gameState of
    Nothing -> putStrLn "None"
    Just amount -> putStrLn $ "Total Pot: " ++ show amount
    
  -- print bets
  putStrLn "\nBets:"
  case bets gameState of
    Nothing -> putStrLn "No bets placed yet."
    Just betList -> mapM_ showBet betList
    

-- helper function to display a Player
showPlayer :: Player -> String
showPlayer p =
  name p ++ " (Chips: " ++ show (chips p) ++ ") " ++
  "(Role: " ++ show (role p) ++ ")" ++
  "\n(Cards: " ++ show (privateCards p) ++ ")"

-- helper function to print the deck of cards
showDeck :: [Card] -> IO ()
showDeck [] = putStrLn "No cards."
showDeck cards = putStrLn $ unwords (map show cards)

-- helper function to print a bet (Player and amount)
showBet :: (Player, Int) -> IO ()
showBet (player, betAmount) = putStrLn $ name player ++ " bet: " ++ show betAmount

pressEnterToContinue :: IO()
pressEnterToContinue = do
  putStrLn "\nPress enter to continue..."
  _ <- getLine
  return ()

setupState :: IO()
setupState = do

  players <- setupPlayers
  putStrLn "Players in the game: "
  mapM_ (putStrLn . showPlayer) players

  pressEnterToContinue

  -- initial game state
  let shuffledDeck = shuffle deck (mkStdGen 42) -- arbitrary seed
      gameState = GameState 
        { players = players,
          currentDeck = shuffledDeck,
          stage = "Setup", 
          communityCards = Nothing,
          pot = Nothing, 
          bets = Nothing
        }

  displayGameState gameState
  pressEnterToContinue

  putStrLn "Dealing cards..."
  let cards = currentDeck gameState
  let (newPlayers, newCards) = dealCardsToAll players cards

  let updatedGameState = gameState { players = newPlayers, currentDeck = newCards }

  displayGameState updatedGameState
  pressEnterToContinue

  putStrLn "Assigning roles..."
  let rolePlayers = assignDealerAndBlinds newPlayers
  let rolesGameState = updatedGameState { players = rolePlayers }

  displayGameState rolesGameState

  putStr "Assigning roles again"
  let role2players = assignDealerAndBlinds rolePlayers
  let roles2gamestate = rolesGameState {players = role2players}
  
  displayGameState roles2gamestate

  putStrLn "finished"

  



-- preflop
-- place initial bets after being dealt
-- all players place a bet - this reduces their number of chips, should return an int bet
-- all of their bets are summed and added to pot


main :: IO()
main = do
  setupState

