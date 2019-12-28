module Main where

import Hex
import Bot
import Error
import JSON

import Control.Monad (when)
import Data.Either (partitionEithers, fromLeft)
import Data.List (nub)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, hPutStr, stderr, stdout)
import System.Exit (exitFailure)

-- #TODO:
-- > Tests

main = do
  args <- getArgs
  case args of
    [redBot, blueBot] -> hexskell (redBot, blueBot) >>= (hPutStr stdout) . show
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <string: Red Bot> <string: Blue Bot>"
      exitFailure


test = do
  hexskell (phBotCode, phBotCode) >>= (hPutStr stdout) . formatOutputAsJSON
  where
    phBotCode = "const empty = getAllCheckers(grid).filter(checker => checker.team === 'neutral'); return empty[0]"

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
nextBoardState' _ (Left x) = return $ Left x
nextBoardState' bots (Right (boardState@(red, blue))) =
  nextBoardState bots boardState
    >>= return . mapLeft ((,) boardState)


performMove :: BoardState -> Checker -> BoardState
performMove boardState@(red, blue) checker =
  let
    isRedTurn = currentAllegiance boardState == Red
    red' =  if isRedTurn     then checker : red    else red
    blue' = if not isRedTurn then checker : blue   else blue
  in
    (red', blue')