module Year2015.Day5 where

import Data.List (isInfixOf)
import Text.Parsec
  ( ParseError,
    Parsec,
    many,
    newline,
    noneOf,
    parse,
    sepBy,
  )
import Util (getData)

input :: IO String
input = getData "data/2015/Day5.txt"

parser :: Parsec String () [String]
parser = sepBy (many $ noneOf ['\n']) newline

p :: String -> Either ParseError [String]
p = parse parser ""

nVowel :: String -> Int
nVowel = foldr (\c n -> if c `elem` ['a', 'e', 'i', 'o', 'u'] then n + 1 else n) 0

hasDouble :: String -> Bool
hasDouble "" = False
hasDouble [c] = False
hasDouble (c1 : c2 : cs) = c1 == c2 || hasDouble (c2 : cs)

hasSubstring :: String -> String -> Bool
hasSubstring s sSub = sSub `isInfixOf` s

hasTwoHeadPairs :: String -> Bool
hasTwoHeadPairs [] = False
hasTwoHeadPairs [c] = False
hasTwoHeadPairs [c1, c2] = False
hasTwoHeadPairs [c1, c2, c3] = False
hasTwoHeadPairs (c1 : c2 : c3 : c4 : cs) =
  ((c1, c2) == (c3, c4))
    || hasTwoHeadPairs (c1 : c2 : c4 : cs)

hasTwoPairs :: String -> Bool
hasTwoPairs s = any hasTwoHeadPairs (take (length s) $ iterate tail s)

hasSymTriple :: String -> Bool
hasSymTriple [] = False
hasSymTriple [c] = False
hasSymTriple [c1, c2] = False
hasSymTriple (c1 : c2 : c3 : cs) = c1 == c3 || hasSymTriple (c2 : c3 : cs)

y2015d5ex1 :: IO ()
y2015d5ex1 = do
  contents <- input
  let ss = case p contents of
        Left _ -> error "Input failed to parse"
        Right strs -> strs
  let ans = length $ filter (\s -> nVowel s >= 3 && hasDouble s && not (any (hasSubstring s) ["ab", "cd", "pq", "xy"])) ss
  print ans

y2015d5ex2 :: IO ()
y2015d5ex2 = do
  contents <- input
  let ss = case p contents of
        Left _ -> error "Input failed to parse"
        Right strs -> strs
  let ans = length $ filter (\s -> hasTwoPairs s && hasSymTriple s) ss
  print ans