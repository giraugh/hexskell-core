module JSON where

import Hex
import Error
import Turn

import Data.List (intersperse)

formatOutputAsJSON :: (Allegiance, BoardState, Maybe BotError, LogRB) -> String
formatOutputAsJSON (winner, bs, error, logs) = if (null error)
    then createJSON $ [w, c, l]
    else createJSON $ [w, e, c, l]
  where
    w = (doubleQuote $ show winner, "winner")
    e = (doubleQuote $ maybe "" getErrorMessage $ error, "error")
    c = (formatBoardStateAsJSON bs, "checkers")
    l = (formatLogsAsJSON logs, "logs")

formatLogsAsJSON :: LogRB -> String
formatLogsAsJSON (red, blue) = "{\"red\":" ++ show red ++ ", \"blue\":" ++ show blue ++ "}"

formatBoardStateAsJSON :: BoardState -> String
formatBoardStateAsJSON bs = createJSON [("[" ++ r ++ "]", "red"), ("[" ++ b ++ "]", "blue")] --["{\"red\": [", r, "], \"blue\": [", b, "]}"]
  where
    checkerToJSON = \(x, y) -> concat $ ["[", show x, ",", show y, "]"]
    checkersToJSON = concat . (intersperse ",") . map checkerToJSON . reverse
    (r, b) = both checkersToJSON bs

doubleQuote :: String -> String
doubleQuote x = "\"" ++ x ++ "\""

createJSON' :: [(String, String)] -> String
createJSON' [] = ""
createJSON' ((value, label):xs) = doubleQuote label ++ ":" ++ value ++ (if null xs then "" else ", " ++ createJSON' xs)


createJSON :: [(String, String)] -> String
createJSON xs = "{" ++ createJSON' xs ++ "}"