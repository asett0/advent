module Year2015.Day6 where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

data Nat = Z | S Nat

data SimpleLight = On | Off deriving (Eq, Show)

type AdvancedLight = Nat

type Coord = (Integer, Integer)

data Action = TurnOn | TurnOff | Toggle

data Instruction = Instr Action Coord Coord

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

input :: IO String
input = U.getData "data/2015/Day6.txt"

actionParser :: Parsec String () Action
actionParser =
  P.try (Toggle <$ P.string "toggle ")
    <|> P.try (TurnOff <$ P.string "turn off ")
    <|> (TurnOn <$ P.string "turn on ")

coordParser :: Parsec String () Coord
coordParser = (,) <$> U.nat <* P.char ',' <*> U.nat

parser :: Parsec String () [Instruction]
parser =
  P.many $ Instr <$> actionParser <*> coordParser <* P.string "through " <*> coordParser

p :: String -> Either ParseError [Instruction]
p = P.parse parser ""

simpleAction :: Action -> SimpleLight -> SimpleLight
simpleAction Toggle On = Off
simpleAction Toggle Off = On
simpleAction TurnOn _ = On
simpleAction TurnOff _ = Off

simpleActionMap :: Instruction -> M.Map Coord (SimpleLight -> SimpleLight)
simpleActionMap (Instr action (lx, ly) (lx', ly')) =
  M.fromList
    [ ( (x, y),
        simpleAction action
      )
      | x <- [min lx lx' .. max lx lx'],
        y <- [min ly ly' .. max ly ly']
    ]

advancedAction :: Action -> AdvancedLight -> AdvancedLight
advancedAction Toggle n = S (S n)
advancedAction TurnOn n = S n
advancedAction TurnOff Z = Z
advancedAction TurnOff (S n) = n

advancedActionMap :: Instruction -> M.Map Coord (AdvancedLight -> AdvancedLight)
advancedActionMap (Instr action (lx, ly) (lx', ly')) =
  M.fromList
    [ ( (x, y),
        advancedAction action
      )
      | x <- [min lx lx' .. max lx lx'],
        y <- [min ly ly' .. max ly ly']
    ]

simpleInit :: M.Map Coord SimpleLight
simpleInit = M.fromList [((x, y), Off) | x <- [0 .. 999], y <- [0 .. 999]]

simpleFinal :: [Instruction] -> M.Map Coord SimpleLight
simpleFinal instructions = M.intersectionWith ($) (foldr (M.unionWith (flip (.)) . simpleActionMap) M.empty instructions) simpleInit

advancedInit :: M.Map Coord AdvancedLight
advancedInit = M.fromList [((x, y), Z) | x <- [0 .. 999], y <- [0 .. 999]]

advancedFinal :: [Instruction] -> M.Map Coord AdvancedLight
advancedFinal instructions = M.intersectionWith ($) (foldr (M.unionWith (flip (.)) . advancedActionMap) M.empty instructions) advancedInit

y2015d6ex1 :: IO ()
y2015d6ex1 = do
  contents <- input
  let instructions = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = M.size $ M.filter (== On) $ simpleFinal instructions
  print ans

y2015d6ex2 :: IO ()
y2015d6ex2 = do
  contents <- input
  let instructions = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = sum $ M.map natToInt $ advancedFinal instructions
  print ans