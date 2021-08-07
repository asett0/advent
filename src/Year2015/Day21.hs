module Year2015.Day21 where

import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

data Character = Character
  { charName :: String,
    hitPoints :: Int,
    charDamage :: Int,
    charArmour :: Int
  }
  deriving (Show)

instance Eq Character where
  (==) you enemy = charName you == charName enemy

data Item = Item
  { itemName :: String,
    cost :: Int,
    itemDamage :: Int,
    itemArmour :: Int
  }
  deriving (Show, Eq, Ord)

input :: IO String
input = U.getData "data/2015/Day21.txt"

parser :: Parsec String () Character
parser =
  let intParser = fromInteger <$> U.nat
   in Character "Boss" <$> (P.string "Hit Points: " *> intParser) <*> (P.string "Damage: " *> intParser) <*> (P.string "Armor: " *> intParser)

p :: String -> Either ParseError Character
p = P.parse parser ""

weapons :: S.Set Item
weapons =
  S.fromList
    [ Item "Dagger" 8 4 0,
      Item "Shortsword" 10 5 0,
      Item "Warhammer" 25 6 0,
      Item "Longsword" 40 7 0,
      Item "Greataxe" 74 8 0
    ]

armours :: S.Set Item
armours =
  S.fromList
    [ Item "Leather" 13 0 1,
      Item "Chainmail" 31 0 2,
      Item "Splintmail" 53 0 3,
      Item "Bandedmail" 75 0 4,
      Item "Platemail" 102 0 5
    ]

rings :: S.Set Item
rings =
  S.fromList
    [ Item "Damage + 1" 25 1 0,
      Item "Damage + 2" 50 2 0,
      Item "Damage + 3" 100 3 0,
      Item "Defense + 1" 20 0 1,
      Item "Defense + 2" 40 0 2,
      Item "Defense + 3" 80 0 3
    ]

equip :: Character -> S.Set Item -> Character
equip = foldl' (\c i -> c {charDamage = charDamage c + itemDamage i, charArmour = charArmour c + itemArmour i})

game :: Character -> Character -> Character
game you enemy =
  let attack = max (charDamage you - charArmour enemy) 1
   in if hitPoints enemy - attack <= 0 then you else game (enemy {hitPoints = hitPoints enemy - attack}) you

validItems :: S.Set Item -> Bool
validItems is =
  S.size (is `S.intersection` weapons) == 1
    && S.size (is `S.intersection` armours) <= 1
    && S.size (is `S.intersection` rings) <= 2

itemsCost :: S.Set Item -> Int
itemsCost = foldl' (\acc item -> acc + cost item) 0

winningItemss :: Character -> Character -> Character -> S.Set (S.Set Item)
winningItemss you enemy winner =
  let itemss = S.powerSet (weapons `S.union` armours `S.union` rings)
   in S.filter (\items -> game (equip you items) enemy == winner) $ S.filter validItems itemss

y2015d21ex1 :: IO ()
y2015d21ex1 = do
  contents <- input
  let boss = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let you = Character {charName = "You", hitPoints = 100, charDamage = 0, charArmour = 0}

  let ans = minimum $ S.map itemsCost $ winningItemss you boss you
  print ans

y2015d21ex2 :: IO ()
y2015d21ex2 = do
  contents <- input
  let boss = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let you = Character {charName = "You", hitPoints = 100, charDamage = 0, charArmour = 0}
  let ans = maximum $ S.map itemsCost $ winningItemss you boss boss
  print ans
