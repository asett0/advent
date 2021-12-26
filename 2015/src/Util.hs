module Util where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Parsec (Parsec)
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (GenTokenParser (decimal, integer, natural), TokenParser)

getData :: String -> IO String
getData fileName = do
  handle <- openFile fileName ReadMode
  hGetContents handle

lexer :: TokenParser ()
lexer = haskell

nat :: Parsec String () Integer
nat = natural lexer

int :: Parsec String () Integer
int = integer lexer

dec :: Parsec String () Integer
dec = decimal lexer