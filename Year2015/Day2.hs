module Year2015.Day2 where

import Text.Parsec (ParseError, Parsec, char, many, parse)
import Util (getData, int)

type Dimensions = (Integer, Integer, Integer)

input :: IO String
input = getData "data/2015/Day2.txt"

parser :: Parsec String () [Dimensions]
parser =
  let x = char 'x'
   in many $ (,,) <$> (int <* x) <*> (int <* x) <*> int

p :: String -> Either ParseError [Dimensions]
p = parse parser ""

wrapping :: Dimensions -> Integer
wrapping (l, w, h) =
  let lw = l * w
      wh = w * h
      hl = h * l
   in 2 * lw + 2 * wh + 2 * hl + minimum [lw, wh, hl]

ribbon :: Dimensions -> Integer
ribbon (l, w, h) = minimum [2 * (l + w), 2 * (w + h), 2 * (h + l)] + l * w * h

ex1 :: IO ()
ex1 = do
  contents <- input
  let dims = case p contents of
        Right res -> res
        Left _ -> error "Input failed to parse"
  let ans = sum $ map wrapping dims
  print ans

ex2 :: IO ()
ex2 = do
  contents <- input
  let dims = case p contents of
        Right res -> res
        Left _ -> error "Input failed to parse"
  let ans = sum $ map ribbon dims
  print ans