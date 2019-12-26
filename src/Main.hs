module Main where

import Hex
import Bot
import Error

-- #TODO:
-- > Handle errors from bots e.g returning a previously taken spot, handled by bot? Maybe bot has 3 strikes then out policy?
-- > Input and output from main
-- > timelimit bot scripts
-- > ensure js sandbox
-- > Tests

--helpers
clamp mi ma x = max (mi) (min ma x)
--

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
  turn <- getLine >>= (return . pred . read) :: IO Int
  stateAt turn