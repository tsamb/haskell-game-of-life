module InputParser where
import Data.Array
import GameTypes

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
charToCell _   = error "Only periods, asterisks and comments are valid input"
