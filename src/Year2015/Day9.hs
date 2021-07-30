module Year2015.Day9 where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

type Loc = String

type Distance = ((String, String), Int)

input :: IO String
input = U.getData "data/2015/Day9.txt"

locParser :: Parsec String () Loc
locParser = P.many P.letter

locPairParser :: P.Parsec String () (Loc, Loc)
locPairParser = (,) <$> (locParser <* P.string " to ") <*> (locParser <* P.string " = ")

distParser :: Parsec String () Distance
distParser = (,) <$> locPairParser <*> (fromInteger <$> U.nat)

distsParser :: Parsec String () (M.Map (Loc, Loc) Int)
distsParser = M.fromList <$> P.many distParser

locsParser :: Parsec String () [Loc]
locsParser = L.nub . concat <$> P.many ((\(x, y) -> [x, y]) <$> locPairParser <* U.nat)

totalDistance :: M.Map (Loc, Loc) Int -> [Loc] -> Int
totalDistance dists locs =
  sum . catMaybes . concat $ [[M.lookup (loc, loc') dists, M.lookup (loc', loc) dists] | (loc, loc') <- zip locs $ tail locs]

p :: Parsec String () a -> String -> a
p parser s = case P.parse parser "" s of
  Left _ -> error "Input failed to parse"
  Right res -> res

y2015d9ex1 :: IO ()
y2015d9ex1 = do
  contents <- input
  let dists = p distsParser contents
  let locs = p locsParser contents
  let paths = L.permutations locs
  let ans = minimum $ map (totalDistance dists) paths
  print ans

y2015d9ex2 :: IO ()
y2015d9ex2 = do
  contents <- input
  let distMap = p distsParser contents
  let locs = p locsParser contents
  let paths = L.permutations locs
  let ans = maximum $ map (totalDistance distMap) paths
  print ans
