module Year2015.Day23 where

import Control.Monad.State.Strict (State)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

data Register = RegA | RegB deriving (Eq, Ord)

data Registers = Registers {regA :: Int, regB :: Int}

data Instruction
  = Hlf Register
  | Tpl Register
  | Inc Register
  | Jmp Int
  | Jie Register Int
  | Jio Register Int

type Instructions = M.Map Int Instruction

input :: IO String
input = U.getData "data/2015/Day23.txt"

parser :: Parsec String () Instructions
parser =
  let intParser = fromInteger <$> U.int
      regParser = RegA <$ P.char 'a' <|> RegB <$ P.char 'b'
   in M.fromList
        <$> ( zip [0 ..]
                <$> P.many
                  ( Hlf <$ P.string "hlf " <*> regParser <* P.newline
                      <|> Tpl <$ P.string "tpl " <*> regParser <* P.newline
                      <|> Inc <$ P.string "inc " <*> regParser <* P.newline
                      <|> P.try (Jmp <$ P.string "jmp " <*> intParser)
                      <|> P.try (Jio <$ P.string "jio " <*> regParser <* P.string ", " <*> intParser)
                      <|> Jie <$ P.string "jie " <*> regParser <* P.string ", " <*> intParser
                  )
            )

getReg :: Register -> Registers -> Int
getReg RegA registers = regA registers
getReg RegB registers = regB registers

alterReg :: Register -> (Int -> Int) -> Registers -> Registers
alterReg RegA f registers = registers {regA = f $ regA registers}
alterReg RegB f registers = registers {regB = f $ regB registers}

computer :: Instructions -> Registers -> Int -> Registers
computer instructions registers k = case M.lookup k instructions of
  Nothing -> registers
  Just (Hlf reg) -> computer instructions (alterReg reg (`div` 2) registers) (k + 1)
  Just (Tpl reg) -> computer instructions (alterReg reg (* 3) registers) (k + 1)
  Just (Inc reg) -> computer instructions (alterReg reg (+ 1) registers) (k + 1)
  Just (Jmp n) -> computer instructions registers (k + n)
  Just (Jie reg n) -> computer instructions registers (k + if even $ getReg reg registers then n else 1)
  Just (Jio reg n) -> computer instructions registers (k + if getReg reg registers == 1 then n else 1)

p :: String -> Either ParseError Instructions
p = P.parse parser ""

y2015d23ex1 :: IO ()
y2015d23ex1 = do
  contents <- input
  let instructions = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = regB $ computer instructions (Registers 0 0) 0
  print ans

y2015d23ex2 :: IO ()
y2015d23ex2 = do
  contents <- input
  let instructions = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = regB $ computer instructions (Registers 1 0) 0
  print ans