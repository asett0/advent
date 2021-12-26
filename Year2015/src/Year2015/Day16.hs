{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Year2015.Day16 where

import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import qualified Data.List as L
import GHC.Generics (Generic)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

data MFCSAMItem
  = Children
  | Cats
  | Samoyeds
  | Pomeranians
  | Akitas
  | Vizslas
  | Goldfish
  | Trees
  | Cars
  | Perfumes
  deriving (Eq, Generic, Show)

instance Hashable MFCSAMItem

input :: IO String
input = U.getData "data/2015/Day16.txt"

mfcsamItemParser :: Parsec String () MFCSAMItem
mfcsamItemParser =
  P.try (Children <$ P.string "children")
    <|> P.try (Cats <$ P.string "cats")
    <|> P.try (Samoyeds <$ P.string "samoyeds")
    <|> P.try (Pomeranians <$ P.string "pomeranians")
    <|> P.try (Akitas <$ P.string "akitas")
    <|> P.try (Vizslas <$ P.string "vizslas")
    <|> P.try (Goldfish <$ P.string "goldfish")
    <|> P.try (Trees <$ P.string "trees")
    <|> P.try (Cars <$ P.string "cars")
    <|> (Perfumes <$ P.string "perfumes")

parser :: Parsec String () [M.HashMap MFCSAMItem Int]
parser =
  let intParser = fromInteger <$> U.int
   in P.many $
        M.fromList <$ P.string "Sue " <* U.nat <* P.string ": "
          <*> ((,) <$> mfcsamItemParser <* P.string ": " <*> intParser) `P.sepBy` P.string ", "

p :: String -> Either ParseError [M.HashMap MFCSAMItem Int]
p = P.parse parser ""

tickerTape :: M.HashMap MFCSAMItem Int
tickerTape =
  M.fromList
    [ (Children, 3),
      (Cats, 7),
      (Samoyeds, 2),
      (Pomeranians, 3),
      (Akitas, 0),
      (Vizslas, 0),
      (Goldfish, 5),
      (Trees, 3),
      (Cars, 2),
      (Perfumes, 1)
    ]

auntSue :: M.HashMap MFCSAMItem Int -> Bool
auntSue sue = and (M.intersectionWith (==) sue tickerTape)

realAuntSue :: M.HashMap MFCSAMItem Int -> Bool
realAuntSue sue =
  and
    ( M.intersectionWithKey
        ( \case
            Cats -> (>)
            Trees -> (>)
            Pomeranians -> (<)
            Goldfish -> (<)
            _ -> (==)
        )
        sue
        tickerTape
    )

y2015d16ex1 :: IO ()
y2015d16ex1 = do
  contents <- input
  let sues = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = case L.findIndex auntSue sues of
        Nothing -> error "Who sent you the present?!"
        Just n -> n + 1
  print ans

y2015d16ex2 :: IO ()
y2015d16ex2 = do
  contents <- input
  let sues = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res
  let ans = case L.findIndex realAuntSue sues of
        Nothing -> error "Who sent you the present?!"
        Just n -> n + 1
  print ans
