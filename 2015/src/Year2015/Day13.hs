module Year2015.Day13 where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

type Name = String

data HappyDir = Gain | Lose deriving (Show)

input :: IO String
input = U.getData "data/2015/Day13.txt"

nameParser :: Parsec String () Name
nameParser = P.many P.letter

dirParser :: Parsec String () HappyDir
dirParser = Lose <$ P.string "lose" <|> Gain <$ P.string "gain"

namesParser :: Parsec String () [Name]
namesParser =
  L.nub . concat
    <$> (`P.sepBy` P.newline)
      ( ( \n1 n2 ->
            [n1, n2]
        )
          <$> nameParser
          <* P.string " would "
          <* dirParser
          <* P.space
          <* (fromInteger <$> U.nat)
          <* P.string "happiness units by sitting next to "
          <*> nameParser
          <* P.char '.'
      )

relsParser :: Parsec String () (M.Map (Name, Name) Int)
relsParser =
  M.fromList
    <$> (`P.sepBy` P.newline)
      ( ( \n1 d v n2 ->
            ( ( n1,
                n2
              ),
              case d of
                Gain -> v
                Lose -> - v
            )
        )
          <$> nameParser
          <* P.string " would "
          <*> dirParser
          <* P.space
          <*> (fromInteger <$> U.nat)
          <* P.string "happiness units by sitting next to "
          <*> nameParser
          <* P.char '.'
      )

p :: Parsec String () a -> String -> a
p parser s = case P.parse parser "" s of
  Left _ -> error "Input failed to parse"
  Right res -> res

happiness :: M.Map (Name, Name) Int -> (Name, Name, Name) -> Int
happiness rels (l, n, r) = fromJust (M.lookup (n, r) rels) + fromJust (M.lookup (n, l) rels)

totalHappiness :: M.Map (Name, Name) Int -> [Name] -> Int
totalHappiness rels names = sum $ map (happiness rels) $ zip3 (last names : init names) names (tail names ++ [head names])

addYourself :: [Name] -> [Name]
addYourself names = if "You" `elem` names then error "You're already at the table!" else "You" : names

addYourRels :: M.Map (Name, Name) Int -> M.Map (Name, Name) Int
addYourRels rels =
  let guests = L.nub $ concatMap (\(n1, n2) -> [n1, n2]) (M.keys rels)
   in M.union
        rels
        ( M.fromList $
            concatMap (\(kv1, kv2) -> [kv1, kv2]) [((("You", guest), 0), ((guest, "You"), 0)) | guest <- guests]
        )

y2015d13ex1 :: IO ()
y2015d13ex1 = do
  contents <- input
  let rels = p relsParser contents
  let names = p namesParser contents
  let ans = maximum $ map (totalHappiness rels) (L.permutations names)
  print ans

y2015d13ex2 :: IO ()
y2015d13ex2 = do
  contents <- input
  let rels = addYourRels $ p relsParser contents
  let names = addYourself $ p namesParser contents
  let ans = maximum $ map (totalHappiness rels) (L.permutations names)
  print ans
