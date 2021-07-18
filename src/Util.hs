module Util where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Parsec (Parsec)
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (GenTokenParser (natural), TokenParser)

getData :: String -> IO String
getData fileName = do
  handle <- openFile fileName ReadMode
  hGetContents handle

lexer :: TokenParser ()
lexer = haskell

int :: Parsec String () Integer
int = natural lexer