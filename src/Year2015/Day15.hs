module Year2015.Day15 where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

data Food = Food
  { capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Show)

type NutritionInfo = M.Map String Food

type Recipe = M.Map String Int

input :: IO String
input = U.getData "data/2015/Day15.txt"

parser :: Parsec String () (M.Map String Food)
parser =
  let intParser = fromInteger <$> U.int
   in M.fromList
        <$> P.many
          ( (,) <$> P.many P.letter
              <*> ( Food <$ P.string ": capacity " <*> intParser
                      <* P.string ", durability " <*> intParser
                      <* P.string ", flavor " <*> intParser
                      <* P.string ", texture " <*> intParser
                      <* P.string ", calories " <*> intParser
                  )
          )

p :: String -> Either ParseError (M.Map String Food)
p = P.parse parser ""

sums :: Int -> Int -> [[Int]]
sums n tot =
  let sumTails = foldr ($) [[]] (replicate (n - 1) $ concatMap extendSum)
   in map (\sumTail -> tot - sum sumTail : sumTail) sumTails
  where
    extendSum :: [Int] -> [[Int]]
    extendSum rest = [x : rest | x <- [0 .. max (tot - sum rest) 0]]

score :: Food -> Int
score food =
  let scores = [capacity food, durability food, flavor food, texture food]
   in if any (0 >=) scores then 0 else product scores

allRecipes :: [String] -> Int -> [Recipe]
allRecipes ss t = map (M.fromList . zip ss) $ sums (length ss) t

cookie :: NutritionInfo -> Recipe -> Food
cookie nutritionInfo recipe =
  Food
    (totalNutrient capacity)
    (totalNutrient durability)
    (totalNutrient flavor)
    (totalNutrient texture)
    (totalNutrient calories)
  where
    totalNutrient :: (Food -> Int) -> Int
    totalNutrient nutrient = sum [fromJust (M.lookup s recipe) * nutrient food | (s, food) <- M.toList nutritionInfo]

y2015d15ex1 :: IO ()
y2015d15ex1 = do
  contents <- input
  let nutritionInfo = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let recipes = allRecipes (M.keys nutritionInfo) 100
  let ans = maximum $ map (score . cookie nutritionInfo) recipes
  print ans

y2015d15ex2 :: IO ()
y2015d15ex2 = do
  contents <- input
  let nutritionInfo = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let recipes = allRecipes (M.keys nutritionInfo) 100
  let ans = maximum $ map score $ filter (\food -> calories food == 500) $ map (cookie nutritionInfo) recipes
  print ans