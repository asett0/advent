{-# LANGUAGE LambdaCase #-}

module Year2015.Day6 where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

data Nat = Z | S Nat
  deriving (Eq)

data Light = Simple Bool | Advanced Nat
  deriving (Eq)

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

enact :: Action -> Light -> Light
enact Toggle (Simple isOn) = Simple (not isOn)
enact Toggle (Advanced brightness) = Advanced $ S $ S brightness
enact TurnOn (Simple _) = Simple True
enact TurnOn (Advanced brightness) = Advanced $ S brightness
enact TurnOff (Simple _) = Simple False
enact TurnOff (Advanced Z) = Advanced Z
enact TurnOff (Advanced (S n)) = Advanced n

actionMap :: Instruction -> M.Map Coord (Light -> Light)
actionMap (Instr action (lx, ly) (lx', ly')) =
  M.fromList
    [ ( (x, y),
        enact action
      )
      | x <- [min lx lx' .. max lx lx'],
        y <- [min ly ly' .. max ly ly']
    ]

initLight :: Light -> M.Map Coord Light
initLight light = M.fromList [((x, y), light) | x <- [0 .. 999], y <- [0 .. 999]]

finalLight :: Light -> [Instruction] -> M.Map Coord Light
finalLight light instructions = M.intersectionWith ($) (foldr (M.unionWith (flip (.)) . actionMap) M.empty instructions) (initLight light)

y2015d6ex1 :: IO ()
y2015d6ex1 = do
  contents <- input
  let instructions = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = M.size $ M.filter (== Simple True) $ finalLight (Simple False) instructions
  print ans

y2015d6ex2 :: IO ()
y2015d6ex2 = do
  contents <- input
  let instructions = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans =
        sum $
          M.map
            ( \case
                Simple _ -> 0
                Advanced n -> natToInt n
            )
            $ finalLight (Advanced Z) instructions
  print ans