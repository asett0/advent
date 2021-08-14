module Year2015.Day25 where

import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day25.txt"

parser :: Parsec String () (Int, Int)
parser =
  let intParser = fromInteger <$> U.nat
   in (,)
        <$ P.string "To continue, please consult the code grid in the manual.  Enter the code at row "
          <*> intParser <* P.string ", column "
          <*> intParser <* P.string "."

p :: String -> Either ParseError (Int, Int)
p = P.parse parser ""

codeN :: Int -> Int -> Integer
codeN r c =
  let diag = r + c
   in fromIntegral $ diag * (diag + 1) `div` 2 + c

genCode :: Int -> Int -> Integer
genCode r c = (20151125 * 252533 ^ codeN r c) `mod` 33554393

y2015d25ex1 :: IO ()
y2015d25ex1 = do
  contents <- input
  let (row, col) = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = genCode (row -1) (col - 1)
  print ans
