module Year2015.Day14 where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

type Name = String

type Reindeer = (Int, Int, Int)

type Time = Int

type Distance = Int

type Points = Int

input :: IO String
input = U.getData "data/2015/Day14.txt"

nameParser :: Parsec String () Name
nameParser = P.many P.letter

parser :: Parsec String () (M.Map Name Reindeer)
parser =
  M.fromList
    <$> (`P.sepBy` P.newline)
      ( (\name speed dur rst -> (name, (speed, dur, rst)))
          <$> nameParser <* P.string " can fly "
          <*> (fromInteger <$> U.nat) <* P.string "km/s for "
          <*> (fromInteger <$> U.nat) <* P.string "seconds, but then must rest for "
          <*> (fromInteger <$> U.nat) <* P.string "seconds."
      )

p :: String -> Either ParseError (M.Map Name Reindeer)
p = P.parse parser ""

move :: Reindeer -> Time -> Int
move (speed, dur, rst) time = let t = time `mod` (dur + rst) in if t > dur || t == 0 then 0 else speed

movements :: Time -> M.Map Name Reindeer -> [M.Map Name Distance]
movements time reindeers = [M.fromList [(name, move reindeer t) | (name, reindeer) <- M.toList reindeers] | t <- [1 .. time]]

distances :: Time -> M.Map Name Reindeer -> M.Map Name Distance
distances time reindeers = M.unionsWith (+) $ movements time reindeers

points :: Time -> M.Map Name Reindeer -> M.Map Name Points
points time reindeers =
  M.unionsWith (+) $
    map (\m -> M.fromList [(name, if cumDist == maximum m then 1 else 0) | (name, cumDist) <- M.toList m]) $
      scanl1 (M.unionWith (+)) $ movements time reindeers

y2015d14ex1 :: IO ()
y2015d14ex1 = do
  contents <- input
  let reindeers = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = maximum $ distances 2503 reindeers
  print ans

y2015d14ex2 :: IO ()
y2015d14ex2 = do
  contents <- input
  let reindeers = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = maximum $ points 2503 reindeers
  print ans