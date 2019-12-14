module Bot where

import Hex

data BotArgument = None | Argument (Checkers, Checkers) BotArgument -- friendly, enemy, previous game state
type Bot = BotArgument -> Checker -- game state -> new piece
type Bots = (Bot, Bot) --red, blue

-- some helpers
both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)
--

-- gamestate is transformed recursively such that friendly checkers are passed first and so that board is transposed for blue
botArgument :: Allegiance -> GameState -> BotArgument
botArgument allegiance (Initial) = None
botArgument allegiance (Ongoing (red, blue) previous) =
  if allegiance == Red
    then Argument (red, blue) (botArgument allegiance previous)
    else Argument (both transposeCoordinates (blue, red)) (botArgument allegiance previous)

-- List of all possible checkers
allCheckers :: Checkers
allCheckers = [(x, y)|x <- [1..11], y <-[1..11]]

positionHasChecker :: BoardState -> Checker -> Bool
positionHasChecker (friendly, enemy) checker = checker `elem` (friendly ++ enemy)

-- placeholder bot (just returns first empty hex)
phBot :: Bot
--phBot (None) =  -- Need to come up with a solution for here, perhaps bots can return a special 'BotError' type?
phBot (Argument (friendly, enemy) previous) =
  head $ filter empty $ allCheckers
  where
    empty = (not . positionHasChecker (friendly, enemy))