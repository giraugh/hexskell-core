module Hex where

import Data.Tuple (swap)
import Data.List

data Allegiance = Neutral | Red | Blue deriving (Enum, Show, Eq)
type Coordinate = (Integer, Integer) 
type Coordinates = [Coordinate]
type Checker = Coordinate
type Checkers = [Checker]
type BoardState = (Checkers, Checkers) -- red, blue
data GameState = Initial | Ongoing | Win Allegiance deriving (Show)

-- helpers
both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)
--

gameIsWon :: BoardState -> Bool
gameIsWon bs = (allegianceHasWon bs Red || allegianceHasWon bs Blue)

gameState :: BoardState -> GameState
gameState ([],[]) = Initial
gameState bs
  | allegianceHasWon bs Red  = Win Red
  | allegianceHasWon bs Blue = Win Blue
  | otherwise                = Ongoing

-- returns who is currently placing a piece (i.e who's turn it is)
currentAllegiance :: BoardState -> Allegiance
currentAllegiance boardState = if even (turnNumber boardState) then Red else Blue

turnNumber :: BoardState -> Int
turnNumber (red, blue) = (length red) + (length blue)

validCoordinate :: Coordinate -> Bool
validCoordinate (x, y) = x >= 1 && y >= 1 && x <= 11 && y <= 11 

isAdjacent :: Coordinate -> Coordinate -> Bool
isAdjacent (x1, y1) (x2, y2) =
  (max (abs dx) (abs dy) == 1) && (signum dx /= signum dy)
  where
    dx = x2 - x1
    dy = y2 - y1

getAdjacent :: Coordinate -> Coordinates
getAdjacent (x, y) = filter validCoordinate $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y + 1), (x + 1, y - 1)]

allCheckers :: Coordinates
allCheckers = [(x, y)|x <- [1..11], y <-[1..11]]

positionHasChecker :: BoardState -> Coordinate -> Bool
positionHasChecker (red, blue) checker = checker `elem` (red ++ blue)

isAllegiance :: BoardState -> Allegiance -> Coordinate -> Bool
isAllegiance (red, blue) allegiance checker
  = case allegiance of
      Neutral -> not (positionHasChecker (red, blue) checker)
      Red     -> checker `elem` red
      Blue    -> checker `elem` blue


floodFill :: (Coordinate -> Coordinates) -> (Coordinate -> Bool) -> Coordinates -> Coordinates -> Coordinates
floodFill _ _ _ [] = []
floodFill next doProgress closed open = open' ++ floodFill next doProgress closed'' open'
  where
    adjoining = nub $ concatMap ((filter doProgress) . next) open
    closed' = closed ++ open
    open' = adjoining \\ closed'
    closed'' = closed' ++ adjoining


allConnected :: BoardState -> Allegiance -> Coordinate -> Coordinates
allConnected bs allegiance checker = floodFill getAdjacent (isAllegiance bs allegiance) [] [checker] 


allegianceHasWon :: BoardState -> Allegiance -> Bool
allegianceHasWon boardState allegiance =
  not . null $
  filter (\checker -> fst checker == 11) $ -- do they reach the other side?
  concatMap (allConnected bs allegiance) $ -- All of their indirectly connected hexs
  filter (isAllegiance bs allegiance) $ -- that are the right allegiance,
  [(1,y)| y <- [1..11]] -- Starting column coords
  where
    bs = if allegiance == Red then boardState else transposeBoardState boardState

transposeCoordinate :: Coordinate -> Coordinate
transposeCoordinate = swap
transposeCoordinates :: Coordinates -> Coordinates
transposeCoordinates = map transposeCoordinate
transposeBoardState :: BoardState -> BoardState
transposeBoardState = both transposeCoordinates
