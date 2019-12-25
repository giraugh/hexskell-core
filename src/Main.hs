module Main where

import Hex
import Bot

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