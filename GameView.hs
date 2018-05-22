module GameView where

import Data.Array

-- START CITATION: https://markhneedham.com/blog/2012/04/03/haskell-print-friendly-representation-of-an-array/
printGrid :: Show a => Array (Int, Int) a -> IO [()]
printGrid grid = sequence $ map (putStrLn . textRepresentation) $ (toSimpleArray grid ++ [[]]) 

toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]]
  where ((lowx, lowy), (highx, highy)) =  bounds grid

textRepresentation :: Show a => [a] -> String
textRepresentation row = foldl (\acc y -> acc ++ (show y) ++ " ") "" row
-- END CITATION
