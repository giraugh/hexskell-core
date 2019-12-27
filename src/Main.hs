module Main where

import Hex
import Bot
import Error

import Control.Monad (when)
import Data.Either (partitionEithers, fromLeft)
import Data.List (nub)

-- #TODO:
-- > Input and output from main
-- > Tests

nextBoardState :: (String, String) -> BoardState -> IO (Either BotError BoardState)  
nextBoardState (redJS, blueJS) boardState@(red, blue) =
  if not $ gameIsWon boardState
    then
      let
        turn = currentAllegiance boardState
        script = if turn == Red then redJS else blueJS
      in
        runExternalBotScript script turn boardState
        >>= return . mapRight (performMove boardState)
    else
      return $ Right boardState

nextBoardState' :: (String, String) -> Either (BoardState, BotError) BoardState -> IO (Either (BoardState, BotError) BoardState)  
nextBoardState' (redJS, blueJS) (Left x) = return $ Left x
nextBoardState' (redJS, blueJS) (Right (boardState@(red, blue))) =
  if not $ gameIsWon boardState
    then
      let
        turn = currentAllegiance boardState
        script = if turn == Red then redJS else blueJS
      in
        runExternalBotScript script turn boardState
        >>= return . mapRight (performMove boardState)
        >>= return . mapLeft ((,) boardState)
    else
      return $ Right boardState

performMove :: BoardState -> Checker -> BoardState
performMove boardState@(red, blue) checker =
  let
    isRedTurn = currentAllegiance boardState == Red
    red' =  if isRedTurn     then checker : red    else red
    blue' = if not isRedTurn then checker : blue   else blue
  in
    (red', blue')

-- takes (left, right) bot code strings
-- returns 
--   > End boardstate
--   > Winning allegiance
--   > A BotError if there was one (which terminates the game)
--   > A set of log messages from red and blue (yet to be implemented, ph for now)
hexskell :: (String, String) -> IO (Allegiance, BoardState, Maybe BotError)
hexskell bots@(red, blue) = do

  -- Read template and create bot scripts
  redS <- botScriptFromBotCode red
  blueS <- botScriptFromBotCode blue
  
  -- iterate states until last
  let scripts = (redS, blueS)
  let state = ([], [])
  finalState <- iterate (>>= nextBoardState' scripts) (return $ Right state) !! 130

  -- Return winning / failing state
  return $ case finalState of
    Left ((final, BotError msg)) -> (opposingAllegiance . currentAllegiance $ final, final, Just (BotError msg))
    Right final -> (winningAllegiance final, final, Nothing)


main = do
  putStrLn "~HEXSKELL~"
  putStrLn "Using placeholder bots."

  hexskell (phBotCode, phBotCode)

  where
    phBotCode = "const empty = getAllCheckers(grid).filter(checker => checker.team === 'neutral'); return empty[0]"