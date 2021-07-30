module Year2015.Day10 where

import qualified Data.List as L
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day10.txt"

p :: String -> Either ParseError Int
p = P.parse (fromInteger <$> U.nat) ""

intToDigs :: Int -> [Int]
intToDigs 0 = []
intToDigs x = intToDigs (x `div` 10) ++ [x `mod` 10]

rlEncode :: [Int] -> [(Int, Int)]
rlEncode = map (\x -> (length x, head x)) . L.group

lookAndSay :: [Int] -> [Int]
lookAndSay digits = concatMap (\(x, y) -> [x, y]) $ rlEncode digits

y2015d10ex1 :: IO ()
y2015d10ex1 = do
  contents <- input
  let seed = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = length $ last $ take 41 $ iterate lookAndSay (intToDigs seed)
  print ans

y2015d10ex2 :: IO ()
y2015d10ex2 = do
  contents <- input
  let seed = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = length $ last $ take 51 $ iterate lookAndSay (intToDigs seed)
  print ans