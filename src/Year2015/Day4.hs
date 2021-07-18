module Year2015.Day4 where

import Crypto.Hash (MD5 (MD5), hashWith)
import Data.ByteString.Char8 (pack)
import Data.Foldable (find)
import Util (getData)

input :: IO String
input = getData "data/2015/Day4.txt"

md5Hash :: String -> String
md5Hash s = show $ hashWith MD5 $ pack s

leadingZeros :: String -> Int
leadingZeros "" = 0
leadingZeros (c : cs) = if c == '0' then 1 + leadingZeros cs else 0

enoughZeros :: String -> Int -> Bool
enoughZeros s n = leadingZeros s >= n

findHash :: String -> Int -> Maybe Int
findHash secretKey n = find (\m -> enoughZeros (md5Hash $ secretKey ++ show m) n) [0 ..]

y2015d4ex1 :: IO ()
y2015d4ex1 = do
  contents <- input
  let ans = maybe "Never finds Hash" show $ findHash contents 5
  putStrLn ans

y2015d4ex2 :: IO ()
y2015d4ex2 = do
  contents <- input
  let ans = maybe "Never finds Hash" show $ findHash contents 6
  putStrLn ans
