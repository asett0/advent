module Year2015.Day3 where

import Data.List (nub)
import Util (getData)

type Position = (Int, Int)

input :: IO String
input = getData "data/2015/Day3.txt"

moveHouse :: Position -> Char -> Position
moveHouse (i, j) '^' = (i, j + 1)
moveHouse (i, j) '>' = (i + 1, j)
moveHouse (i, j) '<' = (i - 1, j)
moveHouse (i, j) 'v' = (i, j - 1)
moveHouse (i, j) _ = (i, j)

ex1 :: IO ()
ex1 = do
  contents <- input
  let ans = length $ nub $ scanl moveHouse (0, 0) contents
  print ans

ex2 :: IO ()
ex2 = do
  contents <- input
  let santa = [x | (i, x) <- zip [0 ..] contents, even i]
  let robot = [x | (i, x) <- zip [0 ..] contents, odd i]
  let ans = length $ nub $ scanl moveHouse (0, 0) santa ++ scanl moveHouse (0, 0) robot
  print ans
