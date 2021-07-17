module Year2015.Day1 where

import Data.List (elemIndex)
import Util (getData)

input :: IO String
input = getData "data/Day1.txt"

convert :: Int -> Char -> Int
convert floor '(' = floor + 1
convert floor ')' = floor - 1
convert floor _ = floor

ex1 :: IO ()
ex1 = do
  contents <- input
  let ans = foldl convert 0 contents
  print ans

ex2 :: IO ()
ex2 = do
  contents <- input
  let ans = elemIndex (-1) (scanl convert 0 contents)
  print ans