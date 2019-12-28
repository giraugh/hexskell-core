module JSON where

import Hex
import Error

import Data.List (intersperse)

formatOutputAsJSON :: (Allegiance, BoardState, Maybe BotError) -> String
formatOutputAsJSON (winner, bs, error) = if (null error)
    then createJSON $ [w, c]
    else createJSON $ [w, e, c]
  where
    w = (show winner, "winner")
    e = (maybe "" getErrorMessage $ error, "error")
    c = (formatBoardStateAsJSON bs, "checkers")


formatBoardStateAsJSON :: BoardState -> String
formatBoardStateAsJSON bs = createJSON [("[" ++ r ++ "]", "red"), ("[" ++ b ++ "]", "blue")] --["{\"red\": [", r, "], \"blue\": [", b, "]}"]
  where
    checkerToJSON = \(x, y) -> concat $ ["[", show x, ",", show y, "]"]
    checkersToJSON = concat . (intersperse ",") . map checkerToJSON . reverse
    (r, b) = both checkersToJSON bs


createJSON' :: [(String, String)] -> String
createJSON' [] = ""
createJSON' ((value, label):xs) = dq label ++ ":" ++ dq value ++ (if null xs then "" else ", " ++ createJSON' xs)
  where dq x = "\"" ++ x ++ "\""


createJSON :: [(String, String)] -> String
createJSON xs = "{" ++ createJSON' xs ++ "}"