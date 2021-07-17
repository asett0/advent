module Year2015.Day1 where

import Data.List
-- import Control.Monad.Identity (Identity)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- import Text.Parsec (ParseError, ParsecT, char, many, parse, (<|>))

-- parser :: ParsecT String () Identity [Char]
-- parser = many $ char '(' <|> char ')'

-- p :: String -> Either ParseError String
-- p = parse parser ""

-- ex1 :: IO ()
-- ex1 = do
--   handle <- openFile "dataDay1.txt" ReadMode
--   contents <- hGetContents handle
--   let ans = case p contents of
--         Right res -> res
--         Left _ -> "Error for ex1"
--   putStrLn ans

-- count :: Eq a => [a] -> a -> Int
-- count xs e = length [x | x <- xs, x == e]

-- getIndex :: [Char] -> Int -> Int -> Maybe Int
-- getIndex [] n i = case n of
--   -1 -> Just i
--   otherwise -> Nothing
-- getIndex (p : ps) n i = case n of
--   -1 -> Just i
--   otherwise -> case p of
--     '(' -> getIndex ps (n + 1) (i + 1)
--     ')' -> getIndex ps (n - 1) (i + 1)
--     otherwise -> Nothing
getInput :: IO String
getInput = do
  handle <- openFile "data/Day1.txt" ReadMode
  hGetContents handle

convert :: Int -> Char -> Int
convert floor '(' = floor + 1
convert floor ')' = floor - 1
convert floor _ = floor

ex1 :: IO ()
ex1 = do
  contents <- getInput
  let ans = foldl convert 0 contents
  print ans

ex2 :: IO ()
ex2 = do
  contents <- getInput
  let ans = elemIndex (-1) (scanl convert 0 contents)
  print ans