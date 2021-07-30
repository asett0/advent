{-# LANGUAGE LambdaCase #-}

module Year2015.Day8 where

import Data.Char as C (digitToInt)
import qualified Text.Parsec as P
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day8.txt"

parser :: Parsec String () [String]
parser = P.many (P.noneOf ['\n']) `P.sepBy` P.newline

p :: Parsec String () a -> String -> String -> a
p parser e s = case P.parse parser "" s of
  Left _ -> error e
  Right res -> res

hexParser :: P.Parsec String () Char
hexParser =
  P.char 'x' *> ((\x y -> toEnum (16 * digitToInt x + digitToInt y)) <$> P.hexDigit <*> P.hexDigit)

charParser :: P.Parsec String () Char
charParser = P.noneOf ['\\', '\"']

decodeEscParser :: P.Parsec String () Char
decodeEscParser = P.char '\\' *> (P.char '\\' <|> P.char '\"' <|> hexParser)

decodeParser :: P.Parsec String () String
decodeParser =
  P.char '\"' *> P.many (charParser <|> decodeEscParser) <* P.char '\"'

encodeEscParser :: P.Parsec String () String
encodeEscParser = ("\\\\" <$ P.char '\\') <|> ("\\\"" <$ P.char '\"')

encodeParser :: P.Parsec String () String
encodeParser = (\sparts -> '\"' : concat sparts ++ "\"") <$> P.many (P.many1 charParser <|> encodeEscParser)

y2015d8ex1 :: IO ()
y2015d8ex1 = do
  contents <- input
  let ss = p parser "Input failed to parse" contents
  let ans = sum (map length ss) - sum (map (length . p decodeParser "Failed to decode") ss)
  print ans

y2015d8ex2 :: IO ()
y2015d8ex2 = do
  contents <- input
  let ss = p parser "Input failed to parse" contents
  let ans = sum (map (length . p encodeParser "Failed to encode") ss) - sum (map length ss)
  print ans
