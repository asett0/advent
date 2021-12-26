module Year2015.Day17 where

import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day17.txt"

parser :: Parsec String () [Int]
parser =
  let intParser = fromInteger <$> U.int
   in P.many intParser

p :: String -> Either ParseError [Int]
p = P.parse parser ""

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = powerset xs ++ map (x :) (powerset xs)

y2015d17ex1 :: IO ()
y2015d17ex1 = do
  contents <- input
  let containers = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = length $ filter (\cs -> sum cs == 150) $ powerset containers
  print ans

y2015d17ex2 :: IO ()
y2015d17ex2 = do
  contents <- input
  let containers = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = let lengths = map length $ filter (\cs -> sum cs == 150) $ powerset containers in length $ filter (== minimum lengths) lengths
  print ans