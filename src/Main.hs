module Main where

-- board is always 11x11
-- two players, red and blue. Red goes first then alternates
-- first to complete a 'bridge' to the opposite side


data Allegiance = Red | Blue deriving (Enum, Show, Eq)

type Coordinate = (Integer, Int)
type Checker = Coordinate
type Checkers = [Checker]
type BoardState = (Checkers, Checkers)
data GameState = Initial | Ongoing BoardState GameState | Win Allegiance deriving (Show) -- Ongoing (Red, Blue, Turn Number, Previous Board State)

-- Bot receives (friendly, enemy, turn, previous) and is always going left -> right from its perspective
type Bot = GameState -> Checker -- game state -> new piece
type Bots = (Bot, Bot) --red, blue

-- placeholder bot
phBot :: Bot
phBot (Ongoing (friendly, enemy) previous) = (1, 1)

currentAllegiance :: Int -> Allegiance
currentAllegiance turnNumber = if even turnNumber then Red else Blue

-- still need to rotate enemy and friendly positions to have friendly always going left -> right
botArgument :: Allegiance -> GameState -> GameState
botArgument allegiance (Ongoing (red, blue) previous) =
  if allegiance == Red
    then (Ongoing (red, blue) previous)
    else (Ongoing (blue, red) previous)



-- still need to add checking for win states
-- currently assumes that bot returned checker is valid
nextGameState :: Bots -> GameState -> GameState
nextGameState bots (Win allegiance) = Win allegiance
nextGameState bots (Initial) =
    Ongoing newBoardState Initial
    where
      newBoardState = (nextBoardState bots (Ongoing ([],[]) Initial))

nextGameState bots currentState@(Ongoing (red, blue) previousState) =
    Ongoing newBoardState previousState
    where
        newBoardState = (nextBoardState bots currentState)


nextBoardState :: Bots -> GameState -> BoardState
nextBoardState (redBot, blueBot) currentState@(Ongoing (red, blue) previous) =
  newState
  where
    turnNumber = length red + length blue
    isRedTurn = currentAllegiance turnNumber == Red
    red' = if isRedTurn then (redBot (botArgument Red currentState) : red) else red
    blue' = if not isRedTurn then (blueBot (botArgument Blue currentState) : blue) else blue
    newState = (red', blue')



main = do
  putStrLn "compiled"