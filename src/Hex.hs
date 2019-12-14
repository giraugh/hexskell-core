module Hex where

import Data.Tuple (swap)

data Allegiance = Red | Blue deriving (Enum, Show, Eq)
type Coordinate = (Integer, Integer)
type Coordinates = [Coordinate]
type Checker = Coordinate
type Checkers = [Checker]
type BoardState = (Checkers, Checkers) -- red, blue
data GameState = Initial | Ongoing BoardState GameState | Win Allegiance deriving (Show)

-- returns who is currently placing a piece (i.e who's turn it is)
currentAllegiance :: Int -> Allegiance
currentAllegiance turnNumber = if even turnNumber then Red else Blue

getBoardState :: GameState -> Maybe BoardState
getBoardState (Initial) = Nothing
getBoardState (Win _) = Nothing
getBoardState (Ongoing boardState _) = Just boardState

getPreviousState :: GameState -> Maybe GameState
getPreviousState (Initial) = Nothing
getPreviousState (Win _) = Nothing
getPreviousState (Ongoing _ previousState) = Just previousState

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

-- Swaps the x and y positions of a list of checkers
transposeCoordinate :: Coordinate -> Coordinate
transposeCoordinate = swap
transposeCoordinates :: Coordinates -> Coordinates
transposeCoordinates = map transposeCoordinate