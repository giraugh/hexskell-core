module Main where

import Hex
import Bot
import Error
import JSON
import Turn

import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, hPutStr, stderr, stdout)
import System.Exit (exitFailure)

-- #TODO:
-- > Tests

main = do
  args <- getArgs
  case args of
    [redBot, blueBot] -> hexskell (redBot, blueBot) >>= (hPutStr stdout) . formatOutputAsJSON
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
--   > A list of log messages for red and blue for each turn
hexskell :: (String, String) -> IO (Allegiance, BoardState, Maybe BotError, LogRB)
hexskell bots@(red, blue) = do

  -- Read template and create bot scripts
  redS <- botScriptFromBotCode red
  blueS <- botScriptFromBotCode blue

  -- iterate states until last
  let scripts = (redS, blueS)
  let state = TurnState ([], []) ([], [])
  finalState <- iterate (>>= nextTurnState' scripts) (return $ Right state) !! 130

  -- Return winning / failing state
  return $ case finalState of
    Left ((TurnState final logs, BotError msg)) -> (opposingAllegiance . currentAllegiance $ final, final, Just (BotError msg), logs)
    Right (TurnState final logs) -> (winningAllegiance final, final, Nothing, logs)