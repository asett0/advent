module Year2015.Day24 where

import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

input :: IO String
input = U.getData "data/2015/Day24.txt"

parser :: Parsec String () [Int]
parser = P.many $ fromInteger <$> U.nat

p :: String -> Either ParseError [Int]
p = P.parse parser ""

-- getReg :: Register -> Registers -> Int
-- getReg RegA registers = regA registers
-- getReg RegB registers = regB registers

-- alterReg :: Register -> (Int -> Int) -> Registers -> Registers
-- alterReg RegA f registers = registers {regA = f $ regA registers}
-- alterReg RegB f registers = registers {regB = f $ regB registers}

-- computer :: Instructions -> Registers -> Int -> Registers
-- computer instructions registers k = case M.lookup k instructions of
--   Nothing -> registers
--   Just (Hlf reg) -> computer instructions (alterReg reg (`div` 2) registers) (k + 1)
--   Just (Tpl reg) -> computer instructions (alterReg reg (* 3) registers) (k + 1)
--   Just (Inc reg) -> computer instructions (alterReg reg (+ 1) registers) (k + 1)
--   Just (Jmp n) -> computer instructions registers (k + n)
--   Just (Jie reg n) -> computer instructions registers (k + if even $ getReg reg registers then n else 1)
--   Just (Jio reg n) -> computer instructions registers (k + if getReg reg registers == 1 then n else 1)

-- groupPackages :: [Int] -> Int -> Maybe [[Int]]
-- groupPackages ps n = let k = sum ps `div` n in
--     groupPackagesHelper ps k n
--     where
--         groupPackagesHelper []
--         groupPackagesHelper (p:ps) k n gs =

kSubsets :: (Eq t, Num t) => t -> [a] -> [[a]]
kSubsets 0 _ = [[]]
kSubsets _ [] = []
kSubsets n (x : xs) = map (x :) (kSubsets (n - 1) xs) ++ kSubsets n xs

y2015d24ex1 :: IO ()
y2015d24ex1 = do
  contents <- input
  let packages = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = reverse packages
  --   let ans = regB $ computer instructions (Registers 0 0) 0
  print ans
