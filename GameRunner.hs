import Game

main = do
  fileContents <- readFile "board.txt"
  putStrLn "Enter the number of evolutions:"
  numIterations <- getLine
  let 
    startBoard = boardFromString fileContents
    evolutions = take (read numIterations) (iterate (evolve) startBoard)
  mapM_ printGrid evolutions
  