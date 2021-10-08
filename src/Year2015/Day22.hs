{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Year2015.Day22 where

import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.Set as S
import qualified Text.Parsec as P
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (Parsec)
import qualified Util as U

-- data Wizard = Wizard
--   { wizHitPoints :: Int,
--     wizArmour :: Int,
--     wizMana :: Int,
--     wizSpent :: Int,
--     wizSpells :: S.Set Spell,
--     wizActiveEffects :: S.Set Spell
--   }
--   deriving (Eq)

-- data Boss = Boss
--   { bossHitPoints :: Int,
--     bossDamage :: Int
--   }
--   deriving (Eq, Show)

-- data Character = W Wizard | B Boss
--   deriving (Eq)

-- instance Ord Character where
--   compare (W w) (W w') = compare (wizSpent w) (wizSpent w')
--   compare (W _) (B _) = LT
--   compare (B _) (W _) = GT
--   compare (B _) (B _) = EQ

-- data GeneralisedSpellType a = Instant | Effect a deriving (Eq, Functor)

-- type SpellType = GeneralisedSpellType Int

-- data Spell = Spell
--   { spellName :: String,
--     spellCost :: Int,
--     spellDamage :: Int,
--     spellHitPoints :: Int,
--     spellArmour :: Int,
--     spellMana :: Int,
--     spellEffect :: SpellType
--   }
--   deriving (Eq)

-- instance Ord Spell where
--   compare = compare `on` spellName

-- data Action = Cast Spell | Attack

-- data GameTree = Move Int GameTree | Won Int | Lost

-- bestGame :: GameTree -> GameTree -> GameTree
-- bestGame Lost x = x
-- bestGame x Lost = x
-- bestGame (Won n) _ = Won n
-- bestGame _ (Won n) = Won n
-- bestGame (Move n x) (Move m y) = case compare n m of
--   LT -> Move n (bestGame x (Move (m - n) y))
--   EQ -> Move n (bestGame x y)
--   GT -> Move m (bestGame (Move (n - m) x) y)

-- input :: IO String
-- input = U.getData "data/2015/Day22.txt"

-- parser :: Parsec String () Boss
-- parser =
--   let intParser = fromInteger <$> U.nat
--    in Boss <$> (P.string "Hit Points: " *> intParser) <*> (P.string "Damage: " *> intParser)

-- p :: String -> Either ParseError Boss
-- p = P.parse parser ""

-- spells :: S.Set Spell
-- spells =
--   S.fromList
--     [ Spell "Magic Missile" 53 4 0 0 0 Instant,
--       Spell "Drain" 73 2 2 0 0 Instant,
--       Spell "Shield" 113 0 0 7 0 (Effect 6),
--       Spell "Poison" 173 3 0 0 0 (Effect 6),
--       Spell "Recharge" 229 0 0 0 101 (Effect 5)
--     ]

-- applySpell :: Wizard -> Boss -> Spell -> (Wizard, Boss)
-- applySpell wiz boss spell = case spellEffect spell of
--   Instant ->
--     ( wiz
--         { wizHitPoints = wizHitPoints wiz + spellHitPoints spell,
--           wizArmour = spellArmour spell,
--           wizMana = wizMana wiz + spellMana spell
--         },
--       boss {bossHitPoints = bossHitPoints boss - spellDamage spell}
--     )
--   Effect _ -> (wiz {wizActiveEffects = S.insert spell $ wizActiveEffects wiz}, boss)

-- move :: Wizard -> Boss -> Action -> (Wizard, Boss)
-- move wiz boss Attack =
--   let attack = max (bossDamage boss - wizArmour wiz) 1
--    in (wiz {wizHitPoints = wizHitPoints wiz - attack}, boss)
-- move wiz boss (Cast spell) = applySpell (wiz {wizMana = wizMana wiz - spellCost spell, wizSpent = wizSpent wiz + spellCost spell}) boss spell

-- applyEffects :: Wizard -> Boss -> (Wizard, Boss)
-- applyEffects wiz boss =
--   let spell =
--         Spell
--           { spellName = "Total",
--             spellCost = 0,
--             spellDamage = total spellDamage,
--             spellHitPoints = total spellHitPoints,
--             spellArmour = total spellArmour,
--             spellMana = total spellMana,
--             spellEffect = Instant
--           }
--    in applySpell
--         ( wiz
--             { wizActiveEffects = S.filter (\spell -> spellEffect spell /= Effect 0) $ S.map (\spell -> spell {spellEffect = subtract 1 <$> spellEffect spell}) $ wizActiveEffects wiz
--             }
--         )
--         boss
--         spell
--   where
--     total :: (Spell -> Int) -> Int
--     total stat = foldl' (\t spell -> t + stat spell) 0 $ wizActiveEffects wiz

-- games :: Character -> Character -> [Character]
-- games (W wiz) (B boss) =
--   let (wiz', boss') = applyEffects wiz boss -- Apply effects
--    in if bossHitPoints boss' <= 0 -- If wizard has not won yet wizard makes a move
--         then [W wiz']
--         else
--           let availableSpells = S.filter (\spell -> wizMana wiz' >= spellCost spell) (difference (wizSpells wiz') (wizActiveEffects wiz')) -- If wizard can make a move, move is for all possible moves
--            in if availableSpells == S.empty
--                 then [B boss']
--                 else
--                   let moves = map (move wiz' boss' . Cast) (S.toList availableSpells) -- For all possible moves, if wizard has not won yet then it's the boss' turn
--                    in concatMap
--                         ( \(wiz'', boss'') ->
--                             if bossHitPoints boss'' <= 0
--                               then [W wiz'']
--                               else games (B boss'') (W wiz'')
--                         )
--                         moves
-- games (B boss) (W wiz) =
--   let (wiz', boss') = applyEffects wiz boss -- Apply effects
--    in if bossHitPoints boss' <= 0 -- If wizard has not won yet boss makes a move
--         then [W wiz']
--         else
--           let (wiz'', boss'') = move wiz' boss' Attack
--            in if wizHitPoints wiz'' <= 0 -- If boss has not won yet then it's the wizard's turn
--                 then [B boss'']
--                 else games (W wiz'') (B boss'')
-- games _ _ = error "Invalid game!"

-- checkSpent :: Character -> Wizard -> Character -> Character
-- checkSpent best wiz c =
--   if W wiz >= best
--     then best
--     else c

-- checkHP :: Character -> Character -> Character -> Character
-- checkHP (B boss) (W wiz) c' =
--   if bossHitPoints boss <= 0
--     then W wiz
--     else c'
-- checkHP (W wiz) (B boss) c' =
--   if wizHitPoints wiz <= 0
--     then B boss
--     else c'
-- checkHP _ _ _ = error "Invalid HP Check"

-- bestGame :: Wizard -> Boss -> Character
-- bestGame wiz boss = bestGameHelper (B boss) (W wiz) (B boss)
--   where
--     bestGameHelper :: Character -> Character -> Character -> Character
--     bestGameHelper best (W wiz) (B boss) =
--       let (wiz', boss') = applyEffects wiz boss -- Apply effects
--        in checkHP (B boss') (W wiz') $
--             let availableSpells = S.filter (\spell -> wizMana wiz' >= spellCost spell) (S.difference (wizSpells wiz') (wizActiveEffects wiz')) -- If wizard can make a move, move is for all possible moves
--              in if availableSpells == S.empty
--                   then B boss'
--                   else
--                     let moves = map (move wiz' boss' . Cast) $ S.toList availableSpells
--                      in -- For all possible moves, if wizard has not won yet then it's the boss' turn
--                         foldl'
--                           ( \b (wiz'', boss'') ->
--                               checkSpent b wiz'' $
--                                 checkHP (B boss'') (W wiz'') $
--                                   bestGameHelper b (B boss'') (W wiz'')
--                           )
--                           best
--                           moves
--     bestGameHelper best (B boss) (W wiz) =
--       let (wiz', boss') = applyEffects wiz boss -- Apply effects
--        in checkHP (B boss') (W wiz') $
--             let (wiz'', boss'') = move wiz' boss' Attack
--              in checkHP (W wiz'') (B boss'') $
--                   bestGameHelper best (W wiz'') (B boss'')
--     bestGameHelper _ _ _ = error "Invalid game!"

-- games (W wiz) (B boss) =
--   let (wiz', boss') = applyEffects wiz boss -- Apply effects
--    in if bossHitPoints boss' <= 0 -- If wizard has not won yet wizard makes a move
--         then [W wiz']
--         else
--           let availableSpells = S.filter (\spell -> wizMana wiz' >= spellCost spell) (difference (wizSpells wiz') (wizActiveEffects wiz')) -- If wizard can make a move, move is for all possible moves
--            in if availableSpells == S.empty
--                 then [B boss']
--                 else
--                   let moves = map (move wiz' boss' . Cast) (S.toList availableSpells) -- For all possible moves, if wizard has not won yet then it's the boss' turn
--                    in concatMap
--                         ( \(wiz'', boss'') ->
--                             if bossHitPoints boss'' <= 0
--                               then [W wiz'']
--                               else games (B boss'') (W wiz'')
--                         )
--                         moves
-- games (B boss) (W wiz) =
--   let (wiz', boss') = applyEffects wiz boss -- Apply effects
--    in if bossHitPoints boss' <= 0 -- If wizard has not won yet boss makes a move
--         then [W wiz']
--         else
--           let (wiz'', boss'') = move wiz' boss' Attack
--            in if wizHitPoints wiz'' <= 0 -- If boss has not won yet then it's the wizard's turn
--                 then [B boss'']
--                 else games (W wiz'') (B boss'')
-- games _ _ = error "Invalid game!"

-- toWizard :: Character -> Wizard
-- toWizard (W wiz) = wiz
-- toWizard (B wiz) = error "Can only apply toWizard to a wizard!"

-- wizards :: [Character] -> [Wizard]
-- wizards = map toWizard . filter isWizard
--   where
--     isWizard :: Character -> Bool
--     isWizard (W _) = True
--     isWizard (B _) = False

-- y2015d22ex1 :: IO ()
-- y2015d22ex1 = do
--   contents <- input
--   let boss = case p contents of
--         Left _ -> error "Input failed to parse"
--         Right res -> res
--   let wiz = Wizard {wizHitPoints = 50, wizArmour = 0, wizMana = 500, wizSpent = 0, wizSpells = spells, wizActiveEffects = S.empty}
--   let ans = wizSpent $ toWizard $ bestGame wiz boss
--   print ans
