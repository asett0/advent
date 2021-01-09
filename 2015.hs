import qualified Data.List as L

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
