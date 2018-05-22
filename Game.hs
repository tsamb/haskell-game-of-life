module Game where
import Data.List (intersect)
import Data.Array
import GameTypes

evolve :: Board -> Board
evolve board = listArray (bounds board) boardValues
  where
    boardCoordinates   = indices board
    boardWithNeighbors = map ((map (board !)) . (intersect boardCoordinates) . neighbors) boardCoordinates
    cellsAndNeighbors  = zip (elems board) boardWithNeighbors
    boardValues        = map evolveCell cellsAndNeighbors

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [

  (x-1,y-1), (x,y-1), (x+1,y-1),
  (x-1,y  ),          (x+1,y),
  (x-1,y+1), (x,y+1), (x+1,y+1)]

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

evolveCell :: (Cell, [Cell]) -> Cell
evolveCell (Alive, neighbors)
  | (countLiving neighbors) == 2 = Alive
  | (countLiving neighbors) == 3 = Alive
  | otherwise                    = Dead

evolveCell (Dead, neighbors)
  | (countLiving neighbors) == 3 = Alive
  | otherwise                    = Dead

countLiving = length . (filter isAlive)
