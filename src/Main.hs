module Main where

import Hex
import Bot
import Error

import Control.Monad (when)

phBotCode = "const empty = getAllCheckers(grid).filter(checker => checker.team === 'neutral'); return empty[0]"

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
        >>= (return . mapRight (performMove boardState))
    else
      return $ Right boardState

nextBoardState' :: (String, String) -> Either BotError BoardState -> IO (Either BotError BoardState)  
nextBoardState' (redJS, blueJS) (Left x) = return $ Left x
nextBoardState' (redJS, blueJS) (Right (boardState@(red, blue))) =
  if not $ gameIsWon boardState
    then
      let
        turn = currentAllegiance boardState
        script = if turn == Red then redJS else blueJS
      in
        runExternalBotScript script turn boardState
        >>= (return . mapRight (performMove boardState))
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


main = do
  putStrLn "~HEXSKELL~"
  putStrLn "Using placeholder bots."

  let state = ([], [])
  phScript <- botScriptFromBotCode phBotCode
  let scripts = (phScript, phScript)
  let next = nextBoardState' scripts
  let stateAt x = iterate (>>= next) (return $ Right state) !! x
  
  putStr "Get boardstate from turn: "
  turn <- getLine >>= (return . (max 0) . read) :: IO Int
  bs <- stateAt turn

  case bs of
    Right bs -> when (gameIsWon bs) $ do
      let redHasWon = allegianceHasWon bs Red
      let message = (if redHasWon then "Red" else "Blue") ++ " has won"
      putStrLn message
    Left x -> return ()

--   return bs