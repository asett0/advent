module Year2015.Day20 where

import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day20.txt"

parser :: Parsec String () Int
parser = fromInteger <$> U.nat

p :: String -> Either ParseError Int
p = P.parse parser ""

primeFactors :: Int -> [(Int, Int)]
primeFactors n = map (\es -> (head es, length es)) $ L.group $ primeFactorsHelper 2 n
  where
    primeFactorsHelper :: Int -> Int -> [Int]
    primeFactorsHelper _ 1 = []
    primeFactorsHelper d n
      | d * d > n = [n]
      | n `mod` d == 0 = d : primeFactorsHelper d (n `div` d)
      | otherwise = primeFactorsHelper (d + 1) n

sumFactors :: Int -> Int
sumFactors n = sumFactorsHelper $ primeFactors n
  where
    sumFactorsHelper :: [(Int, Int)] -> Int
    sumFactorsHelper [] = 1
    sumFactorsHelper ((x, c) : pfs) = ((x ^ (c + 1) -1) `div` (x - 1)) * sumFactorsHelper pfs

adjustedSumFactors :: Int -> Int -> Int
adjustedSumFactors k n = sum [n `div` j | j <- [1 .. k], n `mod` j == 0]

y2015d20ex1 :: IO ()
y2015d20ex1 = do
  contents <- input

  let n = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res

  let ans = fromJust $ L.find (\x -> sumFactors x * 10 >= n) [1 ..]
  print ans

y2015d20ex2 :: IO ()
y2015d20ex2 = do
  contents <- input

  let n = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res

  let ans = fromJust $ L.find (\x -> adjustedSumFactors 50 x * 11 >= n) [1 ..]
  print ans