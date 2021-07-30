module Year2015.Day11 where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day11.txt"

inc :: String -> String
inc = fst . foldr incCarry ("", True)
  where
    incCarry :: Char -> (String, Bool) -> (String, Bool)
    incCarry c (s, False) = (c : s, False)
    incCarry 'z' (s, True) = ('a' : s, True)
    incCarry c (s, True) = (C.chr (C.ord c + 1) : s, False)

validLower :: String -> Bool
validLower = all C.isLower

validLength :: String -> Bool
validLength s = length s == 8

validSequence :: String -> Bool
validSequence [] = False
validSequence [c] = False
validSequence [c1, c2] = False
validSequence (c1 : c2 : c3 : cs) = (C.ord c2 - C.ord c1 == 1 && C.ord c3 - C.ord c2 == 1) || validSequence (c2 : c3 : cs)

validExcludes :: String -> Bool
validExcludes s = all (`notElem` s) ['i', 'o', 'l']

validDups :: String -> Bool
validDups s = any validDupsHelper (take (length s) $ iterate tail s)
  where
    validDupsHelper :: String -> Bool
    validDupsHelper [] = False
    validDupsHelper [c] = False
    validDupsHelper [c1, c2] = False
    validDupsHelper [c1, c2, c3] = False
    validDupsHelper (c1 : c2 : c3 : c4 : cs) =
      ((c1 == c2) && (c3 == c4) && (c1 /= c3))
        || validDupsHelper (c1 : c2 : c4 : cs)

validPassword :: String -> Bool
validPassword s = and [validLower s, validLength s, validSequence s, validExcludes s, validDups s]

nextPassword :: String -> String
nextPassword s = case L.find validPassword (iterate inc s) of
  Nothing -> error "Can't find valid password"
  Just pw -> pw

y2015d11ex1 :: IO ()
y2015d11ex1 = do
  contents <- input
  let ans = nextPassword contents
  print ans

y2015d11ex2 :: IO ()
y2015d11ex2 = do
  contents <- input
  let ans = nextPassword $ inc $ nextPassword contents
  print ans
