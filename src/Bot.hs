module Bot where

import Hex
import Error

import Data.List (intersperse, isInfixOf)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Text.Regex (subRegex, mkRegexWithOpts)
import Text.Read (readMaybe)
import Data.Bifunctor (second)

type BotArgument = BoardState -- has (friendly, enemy) instead of (red, blue)

-- helpers
meetsAll :: a -> [(a -> Bool)] -> Bool
meetsAll x predicates = all ($ x) predicates
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = second
--

phBotCode = "const empty = getAllCheckers(grid).filter(checker => checker.team === 'neutral'); return empty[0]"

toExternalCheckersString :: Checkers -> String
toExternalCheckersString checkers = concat $ intersperse "|" $ map (\(x, y) -> show x ++ "," ++ show y) checkers

fromExternalCheckerString :: String -> Maybe Checker
fromExternalCheckerString str = readMaybe ("(" ++ str ++ ")") :: Maybe Checker

scriptFromTemplate :: String -> String -> String
scriptFromTemplate template code =
  (subRegex
    (mkRegexWithOpts "\\/\\* BOT-START \\*\\/.*\\/\\* BOT-END \\*\\/" False True)
    template
    code
  )

checkerValidityRequirements :: BoardState -> [(Checker -> Bool, String)]
checkerValidityRequirements bs = [
  (validCoordinate, "Checker must be on the board: from 1,1 to 11,11"),
  (isAllegiance bs Neutral, "Target checker position is already occupied")]

botScriptValidityRequirements :: [(String -> Bool, String)]
botScriptValidityRequirements = [
  (not . isInfixOf "require", "Bot script must not contain 'require' keyword")]

isValidNewChecker :: BoardState -> Checker -> Bool
isValidNewChecker boardState checker = checker `meetsAll` predicates
  where
    predicates = map fst $ checkerValidityRequirements boardState

checkerValidityErrors :: BoardState -> Checker -> [BotError]
checkerValidityErrors boardState checker = [ botError label | (pred, label) <- predicatePairs, not $ pred checker ]
  where
    predicatePairs = checkerValidityRequirements boardState

botScriptIsValid :: String -> Bool
botScriptIsValid botJS = botJS `meetsAll` predicates
  where
    predicates = map fst botScriptValidityRequirements

botScriptValidityErrors :: String -> [BotError]
botScriptValidityErrors botJS = [ botError label | (pred, label) <- botScriptValidityRequirements, not $ pred botJS ]


toBotArgument :: Allegiance -> BoardState -> BotArgument
toBotArgument Red (red, blue) = (red, blue)
toBotArgument Blue (red, blue) = transposeBoardState (blue, red)

fromBotReturn :: Allegiance -> Checker -> Checker
fromBotReturn Red = id
fromBotReturn Blue = transposeCoordinate

executeBotScript :: String -> BotArgument -> IO (Either BotError Checker)
executeBotScript script argument@(friendly, enemy) = do

  -- run external process
  (exitCode, out, err) <- readProcessWithExitCode command arguments script

  -- parse output to a checker
  let maybeChecker = fromExternalCheckerString out

  -- was there an error / return approp value?
  return $ case exitCode of
    ExitFailure _ -> Left (BotError $ "Error executing bot:\n" ++ err)
    ExitSuccess -> case maybeChecker of
      Nothing -> Left $ BotError "Bot failed to return a checker"
      Just checker -> if isValidNewChecker argument checker
        then Right checker
        else Left $ combineBotErrors "Bot returned invalid checker:\n" $ checkerValidityErrors argument checker

  where
    command = "node"
    friendlyS = toExternalCheckersString friendly
    enemyS = toExternalCheckersString enemy
    arguments = ["", friendlyS, enemyS]

botScriptFromBotCode :: String -> IO String
botScriptFromBotCode code = do
  -- read template
  template <- readFile "./bot-template.js"

  -- sub in program js code
  let fullScript = scriptFromTemplate template code

  -- return as IO String
  return fullScript

-- requires 'node' in PATH & 'bot-template.js' in cd
runExternalBotScript :: String -> Allegiance -> BoardState -> IO (Either BotError Checker)
runExternalBotScript script allegiance boardState@(red, blue) = do

  -- swap red and blue and transpose based on allegiance and transpose correctly
  let arg = toBotArgument allegiance boardState

  -- is bot valid?
  if botScriptIsValid script
    then (executeBotScript script arg) >>= (return . mapRight (fromBotReturn allegiance))
    else return $ Left $ combineBotErrors "Bot script is invalid:\n" $ botScriptValidityErrors script