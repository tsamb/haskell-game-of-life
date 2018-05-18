import Data.Char (digitToInt)
import Data.Array

main = do
  fileContents <- readFile "board.txt"
  putStrLn "Enter the number of evolutions:"
  numIterations <- getLine
  let 
    startBoard = boardFromString fileContents
    evolutions = take (read numIterations) (iterate (evolve) startBoard)
  mapM_ printGrid evolutions
  
boardFromString :: String -> Array(Int, Int) Int
boardFromString input = listArray ((0,0),((xSize-1),(ySize-1))) boardDigits
  where
    boardDigits = stripNewLines input
    ySize = length (takeWhile ('\n'/=) input)
    xSize = count '\n' input

evolve :: Array (Int, Int) Int -> Array (Int, Int) Int
evolve board = listArray (bounds board) boardValues
  where
    boardWithNeighborCoords = map (neighborsInBounds (bounds board)) (indices board)
    boardWithNeighbors = map (map (board !))  boardWithNeighborCoords
    boardWithNeighborCounts = map (count 1) boardWithNeighbors
    cellsAndNeighbors = zip (elems board) boardWithNeighborCounts
    boardValues = map isAliveInNextGen cellsAndNeighbors

isAliveInNextGen :: (Int, Int) -> Int
isAliveInNextGen (1,0) = 0
isAliveInNextGen (1,1) = 0
isAliveInNextGen (1,2) = 1
isAliveInNextGen (1,3) = 1
isAliveInNextGen (0,3) = 1
isAliveInNextGen _ = 0 

neighborsInBounds :: ((Int,Int),(Int,Int)) -> (Int, Int) -> [(Int, Int)]
neighborsInBounds ((xLowerBound, yLowerBound), (xUpperBound, yUpperBound)) (x,y) =
  filter (\coords -> fst coords >= xLowerBound && snd coords >= yLowerBound && fst coords <= xUpperBound && snd coords <= yUpperBound) allNeighbors
  where allNeighbors = [(x-1,y-1),(x,y-1),(x+1,y-1),(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y)]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)
  
stripNewLines :: [Char] -> [Int]  
stripNewLines multiLineString = [digitToInt x | x <- multiLineString, x /= '\n']

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

-- START CITATION: https://markhneedham.com/blog/2012/04/03/haskell-print-friendly-representation-of-an-array/

printGrid :: Show a => Array (Int, Int) a -> IO [()]
printGrid grid = sequence $ map (putStrLn . textRepresentation) $ (toSimpleArray grid ++ [[]]) 

toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
  where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: Show a => [a] -> String
textRepresentation row = foldl (\acc y -> acc ++ (show y) ++ " ") "" row

-- END CITATION
