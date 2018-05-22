module GameTypes where
  import Data.Array

  type Board = Array Coordinate Cell
  type Coordinate = (Int, Int)
  type Boundary   = (Coordinate, Coordinate)
  data Cell       = Alive | Dead

  instance Show Cell where
    show Alive = "*"
    show Dead  = "."
