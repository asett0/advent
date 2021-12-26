module Year2015.Day1 where

import Data.List (elemIndex)
import Util (getData)

input :: IO String
input = getData "data/2015/Day1.txt"

moveFloor :: Int -> Char -> Int
moveFloor floor '(' = floor + 1
moveFloor floor ')' = floor - 1
moveFloor floor _ = floor

y2015d1ex1 :: IO ()
y2015d1ex1 = do
  contents <- input
  let ans = foldl moveFloor 0 contents
  print ans

y2015d1ex2 :: IO ()
y2015d1ex2 = do
  contents <- input
  let ans = maybe "Never reaches basement floor" show $ elemIndex (- 1) (scanl moveFloor 0 contents)
  putStrLn ans