module Error where

import Data.List (intersperse)

data BotError = BotError String deriving (Show)

botError :: String -> BotError
botError message = BotError message

getErrorMessage :: BotError -> String
getErrorMessage (BotError message) = message

combineBotErrors :: String -> [BotError] -> BotError
combineBotErrors label errors = botError $ label ++ (foldr (++) "" $ intersperse ", " $ map getErrorMessage $ errors)