module Main where

import Hex
import Bot


-- board is always 11x11
-- a hex is adjacent to cartesian, +x,-y and -x,+y. Diag not possible when sign of change in x and change in y are the same
-- two players, red and blue. Red goes first then turn alternates (i.e red on even turn numbers, blue on odd turn numbers)
-- winning player is the first to complete a connected path across the board
-- from game perspective, red is left->right and blue is top->bottom
-- [
-- however, positions are transposed to and from bot function such that bots always attempt left->right
-- in addition, bots always receive the friendly checkers first and then the enemy checkers regardless of colour.
-- ] -> Implemented by haskell bot function that controls scripts
-- positions are indexed from 1 till 11 (i.e x,y c [1..11])

-- #TODO:
-- > How to handle errors from bots e.g returning a previously taken spot, handled by bot?
-- > Loading bots from javascript
-- > Tidying
-- > Tests

nextBoardState :: Bots -> BoardState -> BoardState
nextBoardState (redBot, blueBot) boardState@(red, blue) =
  if not $ gameIsWon boardState
  then
    let
      isRedTurn = currentAllegiance boardState == Red
      red' =  if isRedTurn     then (redBot boardState) : red    else red
      blue' = if not isRedTurn then (blueBot boardState) : blue  else blue
    in
      (red', blue')
  else
    boardState


-- JUST FOR REPL
state = ([], [])
next = nextBoardState (phBot, phBot)
d5 = (!! 5) $ iterate next state
d10 = (!! 10) $ iterate next state
d50 = (!! 50) $ iterate next state
--

main = do
  putStrLn "compiled"