import Data.Bits
import qualified Data.Char as Char
import qualified Data.List as L
import Data.Word

count :: Eq a => [a] -> a -> Int
count xs e = length [x | x <- xs, x == e]

getFloor :: [Char] -> Int
getFloor ps = count ps '(' - count ps ')'

getIndex :: [Char] -> Int -> Int -> Maybe Int
getIndex [] n i = case n of
  -1 -> Just i
  otherwise -> Nothing
getIndex (p : ps) n i = case n of
  -1 -> Just i
  otherwise -> case p of
    '(' -> getIndex ps (n + 1) (i + 1)
    ')' -> getIndex ps (n - 1) (i + 1)
    otherwise -> Nothing

getWrapping :: (Int, Int, Int) -> Int
getWrapping (l, w, h) = 2 * l * w + 2 * w * h + 2 * h * l + min (l * w) (min (w * h) (h * l))

getTotalWrapping :: [(Int, Int, Int)] -> Int
getTotalWrapping lwhs = sum $ map getWrapping lwhs

getRibbon :: (Int, Int, Int) -> Int
getRibbon (l, w, h) = min (2 * l + 2 * w) (min (2 * w + 2 * h) (2 * h + 2 * l)) + (l * w * h)

getTotalRibbon :: [(Int, Int, Int)] -> Int
getTotalRibbon lwhs = sum $ map getRibbon lwhs

moveHouse :: (Int, Int) -> Char -> (Int, Int)
moveHouse (i, j) '^' = (i, j + 1)
moveHouse (i, j) '>' = (i + 1, j)
moveHouse (i, j) '<' = (i - 1, j)
moveHouse (i, j) 'v' = (i, j - 1)
moveHouse _ _ = error "Invalid instruction"

nVisited :: [Char] -> Int
nVisited as = length $ L.nub $ scanl moveHouse (0, 0) as

nVisitedSantOrRobot :: [Char] -> Int
nVisitedSantOrRobot as = length $ L.nub $ housesSanta ++ housesRobot
  where
    housesSanta = scanl moveHouse (0, 0) [a | (i, a) <- zip [1 ..] as, odd i]
    housesRobot = scanl moveHouse (0, 0) [a | (i, a) <- zip [1 ..] as, even i]

nVowels :: [Char] -> Int
nVowels cs = length [c | c <- cs, c `elem` ['a', 'e', 'i', 'o', 'u']]

containsDoubles :: [Char] -> Bool
containsDoubles [] = False
containsDoubles [c] = False
containsDoubles (c1 : c2 : cs) = c1 == c2 || containsDoubles (c2 : cs)

isNice :: [Char] -> Bool
isNice cs = nVowels cs >= 3 && containsDoubles cs && not (any (`L.isInfixOf` cs) ["ab", "cd", "pq", "xy"])

nNice :: [[Char]] -> Int
nNice css = length $ filter isNice css

getPairs :: [a] -> [(a, a)]
getPairs = zip <*> tail

remConDups :: Eq a => [a] -> [a]
remConDups [] = []
remConDups [x] = [x]
remConDups (x1 : x2 : xs)
  | x1 == x2 = remConDups (x2 : xs)
  | otherwise = x1 : remConDups (x2 : xs)

counts :: Eq a => [a] -> [Int]
counts xs = [count xs x | x <- xs]

countNonCons :: Eq a => [a] -> [Int]
countNonCons xs = [count (take (i - 1) xs ++ [xs !! i] ++ drop (i + 2) xs) x | (i, x) <- zip [0 ..] xs]

containsSymTriple :: [Char] -> Bool
containsSymTriple [] = False
containsSymTriple [c] = False
containsSymTriple [c1, c2] = False
containsSymTriple (c1 : c2 : c3 : cs) = c1 == c3 || containsSymTriple (c2 : c3 : cs)

newIsNice :: [Char] -> Bool
newIsNice cs = containsSymTriple cs && any (>= 2) (countNonCons $ getPairs cs)

data Action = TurnOn | TurnOff | Toggle

type Rect = ((Int, Int), (Int, Int))

type LightGrid = [[Bool]]

type NewLightGrid = [[Int]]

slice :: Int -> Int -> [a] -> [a]
slice from to xs
  | from <= to = take (to - from + 1) (drop from xs)
  | otherwise = take (from - to + 1) (drop to xs)

getSubGrid :: Rect -> [[a]] -> [[a]]
getSubGrid ((i1, j1), (i2, j2)) grid = map (slice j1 j2) $ slice i1 i2 grid

procSubGrid :: Action -> LightGrid -> LightGrid
procSubGrid action subgrid = case action of
  TurnOn -> map (map $ const True) subgrid
  TurnOff -> map (map $ const False) subgrid
  Toggle -> map (map not) subgrid

procNewSubGrid :: Action -> NewLightGrid -> NewLightGrid
procNewSubGrid action subgrid = case action of
  TurnOn -> map (map (+ 1)) subgrid
  TurnOff -> map (map (\x -> if x > 0 then x - 1 else 0)) subgrid
  Toggle -> map (map (+ 2)) subgrid

replaceSubGrid :: Rect -> [[a]] -> [[a]] -> [[a]]
replaceSubGrid ((i1, j1), (i2, j2)) subgrid grid =
  take i1 grid
    ++ [take j1 gl ++ sgl ++ drop (j2 + 1) gl | (sgl, gl) <- zip subgrid (slice i1 i2 grid)]
    ++ drop (i2 + 1) grid

procInst :: Action -> Rect -> LightGrid -> LightGrid
procInst action rect grid = replaceSubGrid rect (procSubGrid action (getSubGrid rect grid)) grid

procInput :: LightGrid -> [(Action, Rect)] -> LightGrid
procInput = foldl (\grid (action, rect) -> procInst action rect grid)

newProcInst :: Action -> Rect -> NewLightGrid -> NewLightGrid
newProcInst action rect grid = replaceSubGrid rect (procNewSubGrid action (getSubGrid rect grid)) grid

newProcInput :: NewLightGrid -> [(Action, Rect)] -> NewLightGrid
newProcInput = foldl (\grid (action, rect) -> newProcInst action rect grid)

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
