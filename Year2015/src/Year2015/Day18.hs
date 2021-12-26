module Year2015.Day18 where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

newtype Light = Light Bool deriving (Show, Eq)

type Coord = (Int, Int)

input :: IO String
input = U.getData "data/2015/Day18.txt"

lightParser :: Parsec String () Light
lightParser = isOn <$> (P.char '#' <|> P.char '.')
  where
    isOn :: Char -> Light
    isOn '#' = Light True
    isOn '.' = Light False
    isOn _ = error "Invalid initial light state"

parser :: Parsec String () (M.Map Coord Light)
parser =
  M.fromList . concat . zipWith (\rowi row -> map (\(coli, x) -> ((rowi, coli), x)) row) [0 ..]
    <$> ((zip [0 ..] <$> P.many lightParser) `P.sepBy` P.newline)

neighbours :: Coord -> [Coord]
neighbours (x, y) =
  [ (x - 1, y - 1),
    (x, y -1),
    (x + 1, y -1),
    (x -1, y),
    (x + 1, y),
    (x -1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

corners :: Int -> [Coord]
corners dim = [(0, 0), (dim - 1, 0), (0, dim - 1), (dim - 1, dim - 1)]

animate :: M.Map Coord Light -> M.Map Coord Light
animate lightGrid =
  M.fromList [(coord, animateLight coord light) | (coord, light) <- M.toList lightGrid]
  where
    neighbourLights :: Coord -> [Light]
    neighbourLights c = mapMaybe (`M.lookup` lightGrid) $ neighbours c
    animateLight :: Coord -> Light -> Light
    animateLight c l = case l of
      Light True -> Light $ length (filter (== Light True) $ neighbourLights c) `elem` [2, 3]
      Light False -> Light $ length (filter (== Light True) $ neighbourLights c) == 3

animateNoCorner :: Int -> M.Map Coord Light -> M.Map Coord Light
animateNoCorner dim lightGrid =
  M.fromList [(coord, animateLightNoCorner dim coord light) | (coord, light) <- M.toList lightGrid]
  where
    neighbourLights :: Coord -> [Light]
    neighbourLights c = mapMaybe (`M.lookup` lightGrid) $ neighbours c
    animateLightNoCorner :: Int -> Coord -> Light -> Light
    animateLightNoCorner d c l =
      if c `elem` corners d
        then Light True
        else case l of
          Light True -> Light $ length (filter (== Light True) $ neighbourLights c) `elem` [2, 3]
          Light False -> Light $ length (filter (== Light True) $ neighbourLights c) == 3

p :: String -> Either ParseError (M.Map Coord Light)
p = P.parse parser ""

y2015d18ex1 :: IO ()
y2015d18ex1 = do
  contents <- input
  let lightGrid = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let newLightGrid = foldr ($) lightGrid (replicate 100 animate)
  let ans = length $ M.filter (== Light True) newLightGrid
  print ans

y2015d18ex2 :: IO ()
y2015d18ex2 = do
  contents <- input
  let lightGrid = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let dim = round $ sqrt $ fromIntegral $ M.size lightGrid
  let newInitLightGrid = foldr (\c -> M.insert c $ Light True) lightGrid $ corners dim
  let newLightGrid = foldr ($) newInitLightGrid (replicate 100 $ animateNoCorner dim)
  let ans = length $ M.filter (== Light True) newLightGrid
  print ans