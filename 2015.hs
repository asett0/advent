import Data.Bits
import qualified Data.Char as Char
import qualified Data.List as L
import Data.Word

main = print ans

data Statement = AND String String | OR String String | RSHIFT String Int | LSHIFT String Int | NOT String | ASSIGN String
  deriving (Show)

type Instruction = (Statement, String)

getStatement :: String -> [Instruction] -> Statement
getStatement search ((stmt, s) : insts) = if s == search then stmt else getStatement search insts

isInteger :: String -> Bool
isInteger = all Char.isDigit

eval :: String -> [Instruction] -> Word16
eval s instructions = case getStatement s instructions of
  AND s1 s2 -> (if isInteger s1 then (read s1 :: Word16) else eval s1 instructions) .&. (if isInteger s2 then (read s2 :: Word16) else eval s2 instructions)
  OR s1 s2 -> (if isInteger s1 then (read s1 :: Word16) else eval s1 instructions) .|. (if isInteger s2 then (read s2 :: Word16) else eval s2 instructions)
  RSHIFT s1 x -> shiftR (if isInteger s1 then (read s1 :: Word16) else eval s1 instructions) x
  LSHIFT s1 x -> shiftL (if isInteger s1 then (read s1 :: Word16) else eval s1 instructions) x
  NOT s1 -> complement (if isInteger s1 then (read s1 :: Word16) else eval s1 instructions)
  ASSIGN s1 -> (if isInteger s1 then (read s1 :: Word16) else eval s1 instructions)

insts = [(AND "af" "ah", "ai")]

ans = eval "a" insts