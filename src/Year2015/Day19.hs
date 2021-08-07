{-# LANGUAGE DeriveGeneric #-}

module Year2015.Day19 where

import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics (Generic)
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec, (<|>))
import qualified Util as U

data Atom = Al | Ar | B | C | Ca | F | H | Mg | N | O | P | Si | Th | Ti | E | Rn | Y deriving (Eq, Generic, Show)

type Molecule = [Atom]

instance Hashable Atom

type Replacements = M.HashMap Atom [Molecule]

input :: IO String
input = U.getData "data/2015/Day19.txt"

atomParser :: Parsec String () Atom
atomParser =
  P.try (Al <$ P.string "Al")
    <|> B <$ P.string "B"
    <|> P.try (Ca <$ P.string "Ca")
    <|> F <$ P.string "F"
    <|> H <$ P.string "H"
    <|> Mg <$ P.string "Mg"
    <|> N <$ P.string "N"
    <|> O <$ P.string "O"
    <|> P <$ P.string "P"
    <|> Si <$ P.string "Si"
    <|> P.try (Th <$ P.string "Th")
    <|> Ti <$ P.string "Ti"
    <|> E <$ P.string "e"
    <|> Rn <$ P.string "Rn"
    <|> Y <$ P.string "Y"
    <|> C <$ P.string "C"
    <|> Ar <$ P.string "Ar"

replacementsParser :: Parsec String () Replacements
replacementsParser =
  M.fromListWith (++)
    <$> P.many
      ( (,)
          <$> atomParser <* P.string " => " <*> ((: []) <$> P.many atomParser) <* P.newline
      )

parser :: Parsec String () (Replacements, Molecule)
parser = (,) <$> replacementsParser <* P.newline <*> P.many atomParser

p :: String -> Either ParseError (Replacements, Molecule)
p = P.parse parser ""

calibration :: Replacements -> Molecule -> [Molecule]
calibration _ [] = []
calibration replacements (molHead : molRest) = L.nub $ map (++ molRest) (replace molHead) ++ map (molHead :) (calibration replacements molRest)
  where
    replace :: Atom -> [Molecule]
    replace atom = fromMaybe [] (M.lookup atom replacements)

inverseCalibration :: Replacements -> Molecule -> [Molecule]
inverseCalibration _ [] = []
inverseCalibration replacements molecule@(molHead : molRest) =
  let inverseReplacements = [(mol, atom) | (atom, mols) <- M.toList replacements, mol <- mols]
   in L.nub $ [atom : drop (length mol) molecule | (mol, atom) <- inverseReplacements, mol `L.isPrefixOf` molecule] ++ map (molHead :) (inverseCalibration replacements molRest)

constructions :: Replacements -> Molecule -> [[Molecule]]
constructions _ [] = []
constructions _ [E] = [[[E]]]
constructions replacement molecule =
  let invCals = inverseCalibration replacement molecule
   in map (++ [molecule]) $ concatMap (constructions replacement) invCals

y2015d19ex1 :: IO ()
y2015d19ex1 = do
  contents <- input
  let (replacements, medicine) = case p contents of
        Left _ -> error "Input failed to parse"
        Right res -> res

  let ans = length $ calibration replacements medicine
  print ans