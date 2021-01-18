{-# LANGUAGE BangPatterns #-}

import Control.Monad.Identity (Identity)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  fString <- readFile "inputs/day8_inputs.txt"
  let nStringChars = sum $ map length $ lines fString
  let nMemChars = sumTerminate $ map (Parsec.parse parser "(source)") $ lines fString
  print
    ( case nMemChars of
        Left _ -> nMemChars
        Right x -> Right $ nStringChars - x
    )

sumTerminate :: [Either c Int] -> Either c Int
sumTerminate xs = go 0 xs
  where
    go !accum (x : xs) = case x of
      Left _ -> x
      Right n -> go (accum + n) xs
    go !accum [] = Right accum

hexParser :: Parsec.ParsecT String () Identity String
hexParser = do
  hexHead <- Parsec.char 'x'
  hexVal <- Parsec.count 2 Parsec.hexDigit
  return (hexHead : hexVal)

charParser :: Parsec.ParsecT String () Identity String
charParser = do
  char <- Parsec.noneOf ['\\', '\"']
  return [char]

escapeParser :: Parsec.ParsecT String () Identity String
escapeParser = Parsec.string "\\" >> Parsec.choice [Parsec.string "\\", Parsec.string "\"", hexParser]

parser :: Parsec.ParsecT String () Identity Int
parser = do
  openQuote <- Parsec.char '\"'
  body <- Parsec.many $ Parsec.choice [charParser, escapeParser]
  endQuote <- Parsec.char '\"'
  return $ length body

-- escapeOrEnd <- Parsec.oneOf ['\"', '\\']
-- result <- case escapeOrEnd of
--   '\"' -> return chars
--   '\\' -> do
--     escapeString <- Parsec.choice [Parsec.string "\"", Parsec.string "\\", hexParser]
--     return escapeString
-- singleton :: a -> [a]
-- singleton x = [x]

-- bodyParser :: Parsec.ParsecT String () Identity [String]
-- bodyParser = do
--   chars <- Parsec.many charParser
--   escapes <- Parsec.many escapeParser
--   chars <- Parsec.many charParser

--   return $ chars ++ escapes ++

-- ( case escapeOrEnd of
--     '\\' ->
--       return
--         ( do
--
--         )
--     '\"' -> return chars
--   )
-- return "s"

-- body <- Parsec.many $ Parsec.choice [slashParser, hexParser, quoteParser, charParser]
-- endQuote <- stringEndParser
-- return body

-- parser = Parsec.oneOf

-- subList :: Eq a => [a] -> [a] -> Bool
-- subList [] _ = True
-- subList _ [] = False
-- subList (x : xs) (y : ys)
--   | x == y = subList xs ys
--   | otherwise = subList (x : xs) ys

-- "\" f \\ \" kak \\ x70sn \\ xc4kjri \""