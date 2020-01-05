module Turn where

import Hex
import Bot
import Error

type LogRB = ([Log], [Log])
data TurnState = TurnError BotError | TurnState BoardState LogRB

getTurnBoardState :: TurnState -> BoardState
getTurnBoardState (TurnState boardState _) = boardState

getTurnBotError :: TurnState -> BotError
getTurnBotError (TurnError x) = x

appendLog :: Allegiance -> LogRB -> Log -> LogRB
appendLog turn (red, blue) log
  | turn == Red = (log:red, blue)
  | turn == Blue = (red, log:blue)
  | otherwise = (red, blue)

nextTurnState :: (String, String) -> TurnState -> IO TurnState --IO (Either BotError State)
nextTurnState (redJS, blueJS) state@(TurnError msg) = return state
nextTurnState (redJS, blueJS) state@(TurnState boardState logs@(logR, logB)) =
  if not $ gameIsWon boardState
    then
      let
        turn = currentAllegiance boardState
        script = if turn == Red then redJS else blueJS
      in do
        botOut <- runExternalBotScript script turn boardState
                    >>= return . mapRight (fmap $ performMove boardState)
        return $ either (TurnError) (\(log, board) -> TurnState board (appendLog turn logs log)) $ botOut -- need to split r/b logs
    else
      return state


-- returns next turn state or stores final board state if there is an error
-- left -> (terminal board state, terminating bot error)
-- right -> (current board state)
nextTurnState' :: (String, String) -> Either (TurnState, BotError) TurnState -> IO (Either (TurnState, BotError) TurnState)  
nextTurnState' _ (Left x) = return $ Left x
nextTurnState' bots (Right turnState) = do
  next <- nextTurnState bots turnState
  return $ case next of
    TurnState boardState logs -> Right next
    TurnError botError        -> Left (turnState, botError)
