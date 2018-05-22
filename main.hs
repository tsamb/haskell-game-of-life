import Data.List (intersect)
import Data.Array

type Coordinate = (Int, Int)
type Boundary   = (Coordinate, Coordinate)
type Board      = Array Coordinate Cell
data Cell       = Alive | Dead

instance Show Cell where
  show Alive = "*"
  show Dead  = "."

boardFromString :: String -> Board
boardFromString input = listArray boardBounds boardCells
  where
    boardRows = inputStringToBoardRows input
    boardBounds = boardRowsToBoardBounds boardRows
    boardCells = boardRowsToCells boardRows

inputStringToBoardRows :: String -> [[Char]]
inputStringToBoardRows input = removeComments (removeBlankLines (lines input))

removeComments :: [[Char]] -> [[Char]]
removeComments inputRows = filter (\x -> (head x) /= '!') inputRows

removeBlankLines :: [[Char]] -> [[Char]]
removeBlankLines inputRows = filter (/="") inputRows

boardRowsToBoardBounds :: [[Char]] -> Boundary
boardRowsToBoardBounds boardRows = ((0, 0), (xBoundary, yBoundary))
  where
    xBoundary = length boardRows - 1
    yBoundary = foldl (\max row -> if length row > max then length row else max ) 0 boardRows - 1

boardRowsToCells :: [[Char]] -> [Cell]
boardRowsToCells boardRows = [charToCell x | x <- concat boardRows]

charToCell :: Char -> Cell
charToCell '*' = Alive
charToCell '.' = Dead
charToCell _   = Dead -- TODO: Handle this exception?

evolve :: Board -> Board
evolve board = listArray (bounds board) boardValues
  where
    boardCoordinates   = indices board
    boardWithNeighbors = map ((map (board !)) . (intersect boardCoordinates) . neighbors) boardCoordinates
    cellsAndNeighbors  = zip (elems board) boardWithNeighbors
    boardValues        = map evolveCell cellsAndNeighbors

evolveCell :: (Cell, [Cell]) -> Cell
evolveCell (Alive, neighbors)
  | (countLiving neighbors) == 2 = Alive
  | (countLiving neighbors) == 3 = Alive
  | otherwise                    = Dead

evolveCell (Dead, neighbors)
  | (countLiving neighbors) == 3 = Alive
  | otherwise                    = Dead

countLiving = length . (filter isAlive)

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead = False

neighbors :: Coordinate -> [Coordinate]
neighbors (x, y) = [

  (x-1,y-1), (x,y-1), (x+1,y-1),
  (x-1,y  ),          (x+1,y),
  (x-1,y+1), (x,y+1), (x+1,y+1)]
  

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- START CITATION: https://markhneedham.com/blog/2012/04/03/haskell-print-friendly-representation-of-an-array/

printGrid :: Show a => Array (Int, Int) a -> IO [()]
printGrid grid = sequence $ map (putStrLn . textRepresentation) $ (toSimpleArray grid ++ [[]]) 

toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
  where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: Show a => [a] -> String
textRepresentation row = foldl (\acc y -> acc ++ (show y) ++ " ") "" row

-- END CITATION

main = do
  fileContents <- readFile "board.txt"
  putStrLn "Enter the number of evolutions:"
  numIterations <- getLine
  let 
    startBoard = boardFromString fileContents
    evolutions = take (read numIterations) (iterate (evolve) startBoard)
  mapM_ printGrid evolutions
