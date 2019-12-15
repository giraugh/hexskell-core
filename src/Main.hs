module Main where

import Hex
import Bot

import Data.Maybe

-- board is always 11x11
-- a hex is adjacent to cartesian, +x,-y and -x,+y. Diag not possible when sign of change in x and change in y are the same
-- two players, red and blue. Red goes first then turn alternates (i.e red on even turn numbers, blue on odd turn numbers)
-- winning player is the first to complete a connected path across the board
-- from game perspective, red is left->right and blue is top->bottom
-- however, positions are transposed to and from bot function such that bots always attempt left->right
-- in addition, bots always receive the friendly checkers first and then the enemy checkers regardless of colour.
-- positions are indexed from 1 till 11 (i.e x,y c [1..11])

-- #TODO:
-- > Check for win states
-- > How to handle errors from bots e.g returning a previously taken spot
-- > Loading bots from javascript
-- > Tidying
-- > Tests

-- still need to add checking for win states
-- currently assumes that bot returned checker is valid
nextGameState :: Bots -> GameState -> GameState
nextGameState bots (Win allegiance) = Win allegiance
nextGameState bots (Initial) =
    Ongoing newBoardState Initial
    where
      newBoardState = (nextBoardState bots (Ongoing ([],[]) Initial))

nextGameState bots currentState@(Ongoing (red, blue) previousState) =
    if hasWon
      then Win (if redHasWon then Red else Blue)
      else Ongoing (nextBoardState bots currentState) currentState
    where
        redHasWon = allegianceHasWon (red, blue) Red
        blueHasWon = allegianceHasWon (red, blue) Blue
        hasWon = redHasWon || blueHasWon


nextBoardState :: Bots -> GameState -> BoardState
nextBoardState (redBot, blueBot) currentState@(Ongoing (red, blue) previous) =
  newState
  where
    isRedTurn = currentAllegiance (red, blue) == Red
    red' = if isRedTurn then (redBot (botArgument Red currentState) : red) else red
    blue' = if not isRedTurn then (transposeCoordinate (blueBot (botArgument Blue currentState)) : blue) else blue
    newState = (red', blue')


-- JUST FOR REPL
state = Initial
next = nextGameState (phBot, phBot)
d5 = fromJust $ getBoardState $ (!! 5) $ iterate next state
d10 = fromJust $ getBoardState $ (!! 10) $ iterate next state
d50 = fromJust $ getBoardState $ (!! 50) $ iterate next state
--

main = do
  putStrLn "compiled"