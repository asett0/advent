module Year2015.Day7 where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.Char as C
import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Word as Word
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

data Signal = VAL Word.Word16 | VAR String deriving (Show)

data Gate = ID Signal | AND Signal Signal | OR Signal Signal | NOT Signal | RSHIFT Signal Int | LSHIFT Signal Int deriving (Show)

data Op = And String Signal | Or String Signal | Op String (Word.Word16 -> Word.Word16)

input :: IO String
input = U.getData "data/2015/Day7.txt"

wireParser :: Parsec String () String
wireParser = P.many1 P.lower

signalParser :: Parsec String () Signal
signalParser = (VAR <$> wireParser) <|> (VAL . fromInteger <$> U.nat)

gateParser :: Parsec String () Gate
gateParser =
  P.try (NOT <$> (P.string "NOT" *> P.spaces *> signalParser <* P.spaces <* P.spaces))
    <|> P.try (AND <$> (signalParser <* P.spaces <* P.string "AND") <*> (P.spaces *> signalParser <* P.spaces))
    <|> P.try (OR <$> (signalParser <* P.spaces <* P.string "OR") <*> (P.spaces *> signalParser <* P.spaces))
    <|> P.try (RSHIFT <$> (signalParser <* P.spaces <* P.string "RSHIFT") <*> (fromInteger <$ P.spaces <*> U.nat))
    <|> P.try (LSHIFT <$> (signalParser <* P.spaces <* P.string "LSHIFT") <*> (fromInteger <$ P.spaces <*> U.nat))
    <|> (ID <$> signalParser <* P.spaces)

parser :: Parsec String () (M.Map String Gate)
parser = M.fromList <$> P.many (flip (,) <$> gateParser <*> (P.string "-> " *> wireParser <* P.spaces))

p :: String -> Either ParseError (M.Map String Gate)
p = P.parse parser ""

eval :: Signal -> [Op] -> M.Map String Gate -> Word.Word16
eval (VAR s) ops cmap = case M.lookup s cmap of
  Nothing -> error $ "Error: Wire '" ++ s ++ "' is not found in the provided circuit"
  Just (ID signal) -> eval signal (Op s id : ops) cmap
  Just (NOT signal) -> eval signal (Op s complement : ops) cmap
  Just (AND signal1 signal2) -> eval signal1 (And s signal2 : ops) cmap
  Just (OR signal1 signal2) -> eval signal1 (Or s signal2 : ops) cmap
  Just (RSHIFT signal n) -> eval signal (Op s (`shiftR` n) : ops) cmap
  Just (LSHIFT signal n) -> eval signal (Op s (`shiftL` n) : ops) cmap
eval (VAL x) ops cmap = exec x ops cmap

exec :: Word.Word16 -> [Op] -> M.Map String Gate -> Word.Word16
exec x [] cmap = x
exec x ((Op s f) : ops) cmap = let y = f x in exec y ops (M.insert s (ID (VAL y)) cmap)
exec x ((And s signal) : ops) cmap = eval signal (Op s (x .&.) : ops) cmap
exec x ((Or s signal) : ops) cmap = eval signal (Op s (x .|.) : ops) cmap

y2015d7ex1 :: IO ()
y2015d7ex1 = do
  contents <- input
  let circuit = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = eval (VAR "a") [] circuit
  print ans

y2015d7ex2 :: IO ()
y2015d7ex2 = do
  contents <- input
  let circuit = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ex1 = eval (VAR "a") [] circuit
  let ans = eval (VAR "a") [] $ M.insert "b" (ID (VAL ex1)) circuit
  print ans
